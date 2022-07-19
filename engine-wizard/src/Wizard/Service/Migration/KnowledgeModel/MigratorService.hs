module Wizard.Service.Migration.KnowledgeModel.MigratorService where

import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe
import Data.Time

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.Event.EventLenses
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Acl.AclService
import Wizard.Service.Branch.BranchUtil
import Wizard.Service.Branch.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Migration.KnowledgeModel.Migrator.Migrator
import Wizard.Service.Migration.KnowledgeModel.MigratorMapper
import Wizard.Service.Migration.KnowledgeModel.MigratorValidation
import Wizard.Service.Package.PackageService

getCurrentMigrationDto :: String -> AppContextM MigratorStateDTO
getCurrentMigrationDto branchUuid = do
  checkPermission _KM_UPGRADE_PERM
  ms <- getCurrentMigration branchUuid
  branch <- findBranchById branchUuid
  return $ toDTO ms branch

getCurrentMigration :: String -> AppContextM MigratorState
getCurrentMigration branchUuid = do
  ms <- findMigratorStateByBranchUuid branchUuid
  knowledgeModel <- compileKnowledgeModel (ms ^. resultEvents) (Just $ ms ^. branchPreviousPackageId) []
  let stateWithEvent =
        case ms ^. migrationState of
          (ConflictState (CorrectorConflict Nothing)) ->
            ConflictState . CorrectorConflict . Just . head $ ms ^. targetPackageEvents
          state -> state
  return . (currentKnowledgeModel ?~ knowledgeModel) . (migrationState .~ stateWithEvent) $ ms

createMigration :: String -> MigratorStateCreateDTO -> AppContextM MigratorStateDTO
createMigration bUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    logOutOnlineUsersWhenBranchDramaticallyChanged bUuid
    let targetPkgId = reqDto ^. targetPackageId
    branch <- findBranchById bUuid
    branchData <- findBranchDataById bUuid
    previousPkg <- getPreviousPkg branch
    mergeCheckpointPkgId <- getMergeCheckpointPackageId branch
    forkOfPkgId <- getForkOfPackageId branch
    validateMigrationUniqueness bUuid
    validateIfTargetPackageVersionIsHigher forkOfPkgId targetPkgId
    branchEvents <- getBranchEvents (previousPkg ^. pId) mergeCheckpointPkgId
    targetPkgEvents <- getTargetPackageEvents targetPkgId forkOfPkgId
    km <- compileKnowledgeModel (branchData ^. events) (branch ^. previousPackageId) []
    appUuid <- asks _appContextAppUuid
    now <- liftIO getCurrentTime
    let ms = fromCreateDTO branch previousPkg branchEvents targetPkgId targetPkgEvents km appUuid now
    insertMigratorState ms
    migratedMs <- migrateState ms
    return $ toDTO migratedMs branch
  where
    getBranchEvents = getAllPreviousEventsSincePackageIdAndUntilPackageId
    getTargetPackageEvents = getAllPreviousEventsSincePackageIdAndUntilPackageId
    getPreviousPkg branch =
      case branch ^. previousPackageId of
        Just previousPkgId -> getPackageById previousPkgId
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__BRANCH_PREVIOUS_PKG_ABSENCE
    getMergeCheckpointPackageId branch = do
      mMergeCheckpointPackageId <- getBranchMergeCheckpointPackageId branch
      case mMergeCheckpointPackageId of
        Just mergeCheckpointPackageId -> return mergeCheckpointPackageId
        Nothing -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__BRANCH_MISSING_MERGE_CHECKPOINT_PACKAGE_ID
    getForkOfPackageId branch = do
      mForkOfPackageId <- getBranchForkOfPackageId branch
      case mForkOfPackageId of
        Just forkOfPackageId -> return forkOfPackageId
        Nothing -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__BRANCH_MISSING_FORK_OF_PACKAGE_ID

deleteCurrentMigration :: String -> AppContextM ()
deleteCurrentMigration branchUuid =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    _ <- getCurrentMigration branchUuid
    deleteMigratorStateByBranchUuid branchUuid
    return ()

solveConflictAndMigrate :: String -> MigratorConflictDTO -> AppContextM ()
solveConflictAndMigrate branchUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    ms <- getCurrentMigration branchUuid
    validateMigrationState ms
    validateTargetPackageEvent ms
    validateReqDto (ms ^. migrationState) reqDto
    let stateWithSolvedConflicts = solveConflict ms reqDto
    migrateState stateWithSolvedConflicts
    return ()
  where
    validateMigrationState ms =
      case ms ^. migrationState of
        ConflictState (CorrectorConflict _) -> return ()
        _ -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE
    validateTargetPackageEvent ms =
      case length (ms ^. targetPackageEvents) of
        0 -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE
        _ -> return ()
    validateReqDto (ConflictState (CorrectorConflict (Just e))) reqDto =
      if e ^. uuid' == reqDto ^. originalEventUuid
        then when
               (reqDto ^. action == MCAEdited && isNothing (reqDto ^. event))
               (throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT)
        else throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH

solveAllConflicts :: String -> AppContextM ()
solveAllConflicts branchUuid =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    migratorState <- getCurrentMigration branchUuid
    updatedState <- go migratorState
    updateMigratorState updatedState
    return ()
  where
    go migratorState = do
      case migratorState ^. migrationState of
        RunningState -> return migratorState
        ConflictState (CorrectorConflict mEvent) ->
          case mEvent of
            Just event -> do
              let conflictDto =
                    MigratorConflictDTO
                      { _migratorConflictDTOOriginalEventUuid = event ^. uuid'
                      , _migratorConflictDTOAction = MCAApply
                      , _migratorConflictDTOEvent = Just event
                      }
              nextState <- liftIO $ migrate (solveConflict migratorState conflictDto)
              go nextState
            Nothing -> do
              let updatedMigratorState = migratorState & migrationState .~ ErrorState
              updateMigratorState updatedMigratorState
              return updatedMigratorState
        ErrorState -> return migratorState
        CompletedState -> return migratorState

migrateState :: MigratorState -> AppContextM MigratorState
migrateState ms =
  runInTransaction $ do
    migratedMs <- liftIO $ migrate ms
    updateMigratorState migratedMs
    return migratedMs
