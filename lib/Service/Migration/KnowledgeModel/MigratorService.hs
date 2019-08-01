module Service.Migration.KnowledgeModel.MigratorService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe

import Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Event.EventAccessors
import Model.Migration.KnowledgeModel.MigratorState
import Service.Branch.BranchUtils
import Service.KnowledgeModel.KnowledgeModelService
import Service.Migration.KnowledgeModel.Migrator
import Service.Migration.KnowledgeModel.MigratorMapper
import Service.Migration.KnowledgeModel.MigratorValidation
import Service.Package.PackageService

getCurrentMigrationDto :: String -> AppContextM (Either AppError MigratorStateDTO)
getCurrentMigrationDto branchUuid = heGetCurrentMigration branchUuid $ \ms -> return . Right . toDTO $ ms

getCurrentMigration :: String -> AppContextM (Either AppError MigratorState)
getCurrentMigration branchUuid =
  heFindMigratorStateByBranchUuid branchUuid $ \ms ->
    heCompileKnowledgeModel (ms ^. resultEvents) (Just $ ms ^. branchPreviousPackageId) [] $ \knowledgeModel -> do
      let msWithKnowledgeModel = ms & currentKnowledgeModel .~ Just knowledgeModel
      return . Right $ msWithKnowledgeModel

createMigration :: String -> MigratorStateCreateDTO -> AppContextM (Either AppError MigratorStateDTO)
createMigration bUuid mscDto = do
  let targetPkgId = mscDto ^. targetPackageId
  heFindBranchWithEventsById bUuid $ \branch ->
    heGetPreviousPkg branch $ \previousPkg ->
      heGetMergeCheckpointPackageId branch $ \mergeCheckpointPkgId ->
        heGetForkOfPackageId branch $ \forkOfPkgId ->
          heValidateMigrationUniqueness bUuid $
          heValidateIfTargetPackageVersionIsHigher forkOfPkgId targetPkgId $
          heGetBranchEvents (previousPkg ^. pId) mergeCheckpointPkgId $ \branchEvents ->
            getTargetPackageEvents targetPkgId forkOfPkgId $ \targetPkgEvents ->
              heCompileKnowledgeModel (branch ^. events) (branch ^. previousPackageId) [] $ \km -> do
                let ms = fromCreateDTO branch previousPkg branchEvents targetPkgId targetPkgEvents km
                insertMigratorState ms
                migratedMs <- migrateState ms
                return . Right . toDTO $ migratedMs
  where
    heGetBranchEvents = heGetAllPreviousEventsSincePackageIdAndUntilPackageId
    getTargetPackageEvents = heGetAllPreviousEventsSincePackageIdAndUntilPackageId
    heGetPreviousPkg branch callback =
      case branch ^. previousPackageId of
        Just previousPkgId -> heFindPackageById previousPkgId callback
        Nothing -> return . Left . MigratorError $ _ERROR_KMMT_VALIDATION_MIGRATOR__BRANCH_PREVIOUS_PKG_ABSENCE
    heGetMergeCheckpointPackageId branch callback =
      heGetBranchMergeCheckpointPackageId branch $ \mMergeCheckpointPackageId ->
        case mMergeCheckpointPackageId of
          Just mergeCheckpointPackageId -> callback mergeCheckpointPackageId
          Nothing -> return . Left . MigratorError $ _ERROR_KMMT_MIGRATOR__BRANCH_HAS_TO_HAVE_MERGE_CHECKPOINT
    heGetForkOfPackageId branch callback =
      heGetBranchForkOfPackageId branch $ \mForkOfPackageId ->
        case mForkOfPackageId of
          Just forkOfPackageId -> callback forkOfPackageId
          Nothing ->
            return . Left . MigratorError $
            _ERROR_KMMT_MIGRATOR__BRANCH_HAS_TO_HAVE_CHECKPOINT_ABOUT_MERGED_PREVIOUS_PKG

deleteCurrentMigration :: String -> AppContextM (Maybe AppError)
deleteCurrentMigration branchUuid =
  hmGetCurrentMigration branchUuid $ \_ -> do
    deleteMigratorStateByBranchUuid branchUuid
    return Nothing

solveConflictAndMigrate :: String -> MigratorConflictDTO -> AppContextM (Maybe AppError)
solveConflictAndMigrate branchUuid reqDto =
  hmGetCurrentMigration branchUuid $ \ms ->
    validateMigrationState ms $
    validateTargetPackageEvent ms $
    validateReqDto (ms ^. migrationState) reqDto $ do
      let stateWithSolvedConflicts = solveConflict ms reqDto
      migrateState stateWithSolvedConflicts
      return Nothing
  where
    validateMigrationState ms callback =
      case ms ^. migrationState of
        ConflictState (CorrectorConflict _) -> callback
        _ -> return . Just . MigratorError $ _ERROR_KMMT_MIGRATOR__NO_CONFLICTS_TO_SOLVE
    validateTargetPackageEvent ms callback =
      case length (ms ^. targetPackageEvents) of
        0 -> return . Just . MigratorError $ _ERROR_KMMT_MIGRATOR__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE
        _ -> callback
    validateReqDto (ConflictState (CorrectorConflict e)) reqDto callback =
      if getEventUuid' e == reqDto ^. originalEventUuid
        then if reqDto ^. action == MCAEdited && isNothing (reqDto ^. event)
               then return . Just . MigratorError $ _ERROR_KMMT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
               else callback
        else return . Just . MigratorError $
             _ERROR_KMMT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT

migrateState :: MigratorState -> AppContextM MigratorState
migrateState ms = do
  migratedMs <- liftIO $ migrate ms
  updateMigratorState migratedMs
  return migratedMs

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetCurrentMigration branchUuid callback = do
  eitherMigratorState <- getCurrentMigration branchUuid
  case eitherMigratorState of
    Right migratorState -> callback migratorState
    Left error -> return . Left $ error

hmGetCurrentMigration branchUuid callback = do
  eitherMigratorState <- getCurrentMigration branchUuid
  case eitherMigratorState of
    Right migratorState -> callback migratorState
    Left error -> return . Just $ error
