module Service.Migrator.MigratorService where

import Control.Lens ((&), (.~), (^.), makeLenses)
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.UUID as U

import Api.Resource.Migrator.MigratorConflictDTO
import Api.Resource.Migrator.MigratorStateCreateDTO
import Api.Resource.Migrator.MigratorStateDTO
import Common.Context
import Common.Error
import Common.Localization
import Database.DAO.Branch.BranchDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState
import Model.Package.Package
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.Migrator.Applicator
import Service.Migrator.CleanerMethod
import Service.Migrator.CorrectorMethod
import Service.Migrator.Migrator
import Service.Migrator.MigratorMapper
import Service.Package.PackageService

getCurrentMigration :: Context -> String -> IO (Either AppError MigratorStateDTO)
getCurrentMigration context branchUuid = getMigrationState context branchUuid $ \ms -> return . Right . toDTO $ ms

createMigration :: Context -> String -> MigratorStateCreateDTO -> IO (Either AppError MigratorStateDTO)
createMigration context branchUuid mscDto = do
  let targetPackageId = mscDto ^. mscdtoTargetPackageId
  getBranch branchUuid $ \branch ->
    validateIfMigrationAlreadyExist $
    getParentPackageId branch $ \branchParentId ->
      validateIfTargetPackageVersionIsHigher branch targetPackageId $
      getTargetParentPackage targetPackageId $ \targetParentPackage ->
        getBranchEvents branch $ \branchEvents ->
          getTargetParentEvents targetPackageId branch $ \targetPackageEvents -> do
            let ms =
                  MigratorState
                  { _msBranchUuid = branch ^. bwkmUuid
                  , _msMigrationState = RunningState
                  , _msBranchParentId = branchParentId
                  , _msTargetPackageId = targetPackageId
                  , _msBranchEvents = branchEvents
                  , _msTargetPackageEvents = targetPackageEvents
                  , _msResultEvents = []
                  , _msCurrentKnowledgeModel = branch ^. bwkmKM
                  }
            insertMigratorState context ms
            migratedMs <- migrateState context ms
            return . Right . toDTO $ migratedMs
  where
    getBranch branchUuid callback = do
      eitherBranch <- findBranchWithKMByBranchId context branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left (NotExistsError _) -> return . Left . MigratorError $ _ERROR_MT_VALIDATION_MIGRATOR__SOURCE_BRANCH_ABSENCE
        Left error -> return . Left $ error
    validateIfMigrationAlreadyExist callback = do
      eitherMigratorState <- findMigratorStateByBranchUuid context branchUuid
      case eitherMigratorState of
        Right migrationState -> return . Left . MigratorError $ _ERROR_MT_VALIDATION_MIGRATOR__MIGRATION_UNIQUENESS
        Left (NotExistsError _) -> callback
        Left error -> return . Left $ error
    validateIfTargetPackageVersionIsHigher branch targetPackageId callback =
      getLastAppliedParentPackageId branch $ \lastAppliedParentPackageId -> do
        let targetPackageVersion = T.unpack $ splitPackageId targetPackageId !! 2
        let lastAppliedParentPackageVersion = T.unpack $ splitPackageId lastAppliedParentPackageId !! 2
        if isNothing $ isVersionHigher targetPackageVersion lastAppliedParentPackageVersion
          then callback
          else return . Left . MigratorError $ _ERROR_MT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER
    getTargetParentPackage targetPackageId callback = do
      eitherTargetParentPackage <- findPackageWithEventsById context targetPackageId
      case eitherTargetParentPackage of
        Right targetParentPackage -> callback targetParentPackage
        Left (NotExistsError _) ->
          return . Left . MigratorError $ _ERROR_MT_VALIDATION_MIGRATOR__TARGET_PARENT_PKG_ABSENCE
        Left error -> return . Left $ error
    getBranchEvents branch callback =
      getParentPackageId branch $ \parentPackageId ->
        getLastMergeCheckpointPackageId branch $ \lastMergeCheckpointPackageId -> do
          let since = parentPackageId
          let until = lastMergeCheckpointPackageId
          eitherEvents <- getAllPreviousEventsSincePackageIdAndUntilPackageId context since until
          case eitherEvents of
            Right events -> callback events
            Left error -> return . Left $ error
    getParentPackageId branch callback =
      case branch ^. bwkmParentPackageId of
        Just parentPackageId -> callback parentPackageId
        Nothing -> return . Left . MigratorError $ _ERROR_MT_VALIDATION_MIGRATOR__BRANCH_PARENT_ABSENCE
    getLastMergeCheckpointPackageId branch callback =
      case branch ^. bwkmLastMergeCheckpointPackageId of
        Just lastMergeCheckpointPackageId -> callback lastMergeCheckpointPackageId
        Nothing -> return . Left . MigratorError $ _ERROR_MT_MIGRATOR__BRANCH_HAS_TO_HAVE_MERGE_CHECKPOINT
    getTargetParentEvents targetPackageId branch callback =
      getLastAppliedParentPackageId branch $ \lastAppliedParentPackageId -> do
        let since = targetPackageId
        let until = lastAppliedParentPackageId
        eitherEvents <- getAllPreviousEventsSincePackageIdAndUntilPackageId context since until
        case eitherEvents of
          Right events -> callback events
          Left error -> return . Left $ error
    getLastAppliedParentPackageId branch callback =
      case branch ^. bwkmLastAppliedParentPackageId of
        Just lastAppliedParentPackageId -> callback lastAppliedParentPackageId
        Nothing ->
          return . Left . MigratorError $
          _ERROR_MT_MIGRATOR__BRANCH_HAS_TO_HAVE_CHECKPOINT_ABOUT_LAST_MERGED_PARENT_PKG

deleteCurrentMigration :: Context -> String -> IO (Maybe AppError)
deleteCurrentMigration context branchUuid = do
  eitherMigrationState <- findMigratorStateByBranchUuid context branchUuid
  case eitherMigrationState of
    Right _ -> do
      deleteMigratorStateByBranchUuid context branchUuid
      return Nothing
    Left error -> return . Just $ error

solveConflictAndMigrate :: Context -> String -> MigratorConflictDTO -> IO (Maybe AppError)
solveConflictAndMigrate context branchUuid reqDto = do
  eitherMigratorState <- findMigratorStateByBranchUuid context branchUuid
  case eitherMigratorState of
    Right ms ->
      validateMigrationState ms $
      validateTargetPackageEvent ms $
      validateReqDto (ms ^. msMigrationState) reqDto $ do
        let stateWithSolvedConflicts = solveConflict ms reqDto
        migrateState context stateWithSolvedConflicts
        return Nothing
    Left error -> return . Just $ error
  where
    validateMigrationState ms callback =
      case ms ^. msMigrationState of
        ConflictState (CorrectorConflict _) -> callback
        _ -> return . Just . MigratorError $ _ERROR_MT_MIGRATOR__NO_CONFLICTS_TO_SOLVE
    validateTargetPackageEvent ms callback =
      case length (ms ^. msTargetPackageEvents) of
        0 -> return . Just . MigratorError $ _ERROR_MT_MIGRATOR__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE
        _ -> callback
    validateReqDto (ConflictState (CorrectorConflict event)) reqDto callback =
      if getEventUuid event == reqDto ^. mcdtoOriginalEventUuid
        then if reqDto ^. mcdtoAction == MCAEdited && isNothing (reqDto ^. mcdtoEvent)
               then return . Just . MigratorError $ _ERROR_MT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
               else callback
        else return . Just . MigratorError $
             _ERROR_MT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT

getMigrationState context branchUuid callback = do
  eitherMigratorState <- findMigratorStateByBranchUuid context branchUuid
  case eitherMigratorState of
    Right migrationState -> callback migrationState
    Left error -> return . Left $ error

migrateState :: Context -> MigratorState -> IO MigratorState
migrateState context ms = do
  migratedMs <- migrate ms
  updateMigratorState context migratedMs
  return migratedMs
