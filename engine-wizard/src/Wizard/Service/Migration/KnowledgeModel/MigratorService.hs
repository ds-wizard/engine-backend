module Wizard.Service.Migration.KnowledgeModel.MigratorService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe

import Shared.Model.Error.Error
import Shared.Model.Event.EventAccessors
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.LensesConfig
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchUtils
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Migration.KnowledgeModel.Migrator.Migrator
import Wizard.Service.Migration.KnowledgeModel.MigratorMapper
import Wizard.Service.Migration.KnowledgeModel.MigratorValidation
import Wizard.Service.Package.PackageService

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
        Nothing -> return . Left . UserError $ _ERROR_VALIDATION__BRANCH_PREVIOUS_PKG_ABSENCE
    heGetMergeCheckpointPackageId branch callback =
      heGetBranchMergeCheckpointPackageId branch $ \mMergeCheckpointPackageId ->
        case mMergeCheckpointPackageId of
          Just mergeCheckpointPackageId -> callback mergeCheckpointPackageId
          Nothing -> return . Left . UserError $ _ERROR_SERVICE_MIGRATION_KM__BRANCH_MISSING_MERGE_CHECKPOINT_PACKAGE_ID
    heGetForkOfPackageId branch callback =
      heGetBranchForkOfPackageId branch $ \mForkOfPackageId ->
        case mForkOfPackageId of
          Just forkOfPackageId -> callback forkOfPackageId
          Nothing -> return . Left . UserError $ _ERROR_SERVICE_MIGRATION_KM__BRANCH_MISSING_FORK_OF_PACKAGE_ID

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
        _ -> return . Just . UserError $ _ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE
    validateTargetPackageEvent ms callback =
      case length (ms ^. targetPackageEvents) of
        0 -> return . Just . UserError $ _ERROR_SERVICE_MIGRATION_KM__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE
        _ -> callback
    validateReqDto (ConflictState (CorrectorConflict e)) reqDto callback =
      if getEventUuid' e == reqDto ^. originalEventUuid
        then if reqDto ^. action == MCAEdited && isNothing (reqDto ^. event)
               then return . Just . UserError $ _ERROR_SERVICE_MIGRATION_KM__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
               else callback
        else return . Just . UserError $ _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH

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
