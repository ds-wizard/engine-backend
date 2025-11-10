module Wizard.Service.KnowledgeModel.Migration.MigrationService where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import qualified Wizard.Service.KnowledgeModel.Editor.EditorMapper as EditorMapper
import Wizard.Service.KnowledgeModel.Editor.EditorUtil
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.KnowledgeModel.Migration.MigrationAudit
import Wizard.Service.KnowledgeModel.Migration.MigrationMapper
import Wizard.Service.KnowledgeModel.Migration.MigrationValidation
import Wizard.Service.KnowledgeModel.Migration.Migrator.Migrator
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

getCurrentMigrationDto :: U.UUID -> AppContextM KnowledgeModelMigrationDTO
getCurrentMigrationDto editorUuid = do
  checkPermission _KM_UPGRADE_PERM
  ms <- getCurrentMigration editorUuid
  editor <- findKnowledgeModelEditorByUuid editorUuid
  return $ toDTO ms editor

getCurrentMigration :: U.UUID -> AppContextM KnowledgeModelMigration
getCurrentMigration editorUuid = do
  ms <- findMigratorStateByEditorUuid editorUuid
  knowledgeModel <- compileKnowledgeModel ms.resultEvents (Just ms.editorPreviousPackageId) []
  let stateWithEvent =
        case ms.state of
          (ConflictKnowledgeModelMigrationState Nothing) -> ConflictKnowledgeModelMigrationState . Just . head $ ms.targetPackageEvents
          state -> state
  return $ ms {currentKnowledgeModel = Just knowledgeModel, state = stateWithEvent} :: AppContextM KnowledgeModelMigration

createMigration :: U.UUID -> KnowledgeModelMigrationCreateDTO -> AppContextM KnowledgeModelMigrationDTO
createMigration kmEditorUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditorUuid
    let targetPkgId = reqDto.targetPackageId
    editor <- findKnowledgeModelEditorByUuid kmEditorUuid
    previousPkg <- getPreviousPkg editor
    mergeCheckpointPkgId <- getMergeCheckpointPackageId editor
    forkOfPkgId <- getForkOfPackageId editor
    validateMigrationUniqueness kmEditorUuid
    validateIfTargetPackageVersionIsHigher forkOfPkgId targetPkgId
    editorEvents <- getEditorEvents previousPkg.pId mergeCheckpointPkgId
    targetPkgEvents <- getTargetPackageEvents targetPkgId forkOfPkgId
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
    let kmEvents = fmap EditorMapper.toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents editor.previousPackageId []
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let ms = fromCreateDTO editor previousPkg editorEvents targetPkgId targetPkgEvents km tenantUuid now
    insertMigratorState ms
    migratedMs <- migrateState ms
    auditKmMigrationCreate reqDto editor
    return $ toDTO migratedMs editor
  where
    getEditorEvents = getAllPreviousEventsSincePackageIdAndUntilPackageId
    getTargetPackageEvents = getAllPreviousEventsSincePackageIdAndUntilPackageId
    getPreviousPkg editor =
      case editor.previousPackageId of
        Just previousPkgId -> getPackageById previousPkgId
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__KM_EDITOR_PREVIOUS_PKG_ABSENCE
    getMergeCheckpointPackageId editor = do
      mMergeCheckpointPackageId <- getEditorMergeCheckpointPackageId editor
      case mMergeCheckpointPackageId of
        Just mergeCheckpointPackageId -> return mergeCheckpointPackageId
        Nothing -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__EDITOR_MISSING_MERGE_CHECKPOINT_PACKAGE_ID
    getForkOfPackageId editor = do
      mForkOfPackageId <- getEditorForkOfPackageId editor
      case mForkOfPackageId of
        Just forkOfPackageId -> return forkOfPackageId
        Nothing -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__EDITOR_MISSING_FORK_OF_PACKAGE_ID

deleteCurrentMigration :: U.UUID -> AppContextM ()
deleteCurrentMigration editorUuid =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    _ <- getCurrentMigration editorUuid
    deleteMigratorStateByEditorUuid editorUuid
    auditKmMigrationCancel editorUuid
    return ()

solveConflictAndMigrate :: U.UUID -> KnowledgeModelMigrationResolutionDTO -> AppContextM ()
solveConflictAndMigrate editorUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    ms <- getCurrentMigration editorUuid
    validateMigrationState ms
    validateTargetPackageEvent ms
    validateReqDto ms.state reqDto
    let stateWithSolvedConflicts = solveConflict ms reqDto
    migrateState stateWithSolvedConflicts
    auditKmMigrationSolve editorUuid reqDto
    return ()
  where
    validateMigrationState ms =
      case ms.state of
        ConflictKnowledgeModelMigrationState _ -> return ()
        _ -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE
    validateTargetPackageEvent ms =
      case length ms.targetPackageEvents of
        0 -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE
        _ -> return ()
    validateReqDto (ConflictKnowledgeModelMigrationState (Just e)) reqDto =
      when
        (e.uuid /= reqDto.originalEventUuid)
        (throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH)
    validateReqDto _ _ = error "Expected a ConflictKnowledgeModelMigrationState with CorrectorConflict containing an event"

solveAllConflicts :: U.UUID -> AppContextM ()
solveAllConflicts editorUuid =
  runInTransaction $ do
    checkPermission _KM_UPGRADE_PERM
    migratorState <- getCurrentMigration editorUuid
    updatedState <- go migratorState
    updateMigratorState updatedState
    auditKmMigrationApplyAll editorUuid
    return ()
  where
    go migratorState = do
      case migratorState.state of
        RunningKnowledgeModelMigrationState -> return migratorState
        ConflictKnowledgeModelMigrationState mEvent ->
          case mEvent of
            Just event -> do
              let conflictDto =
                    KnowledgeModelMigrationResolutionDTO
                      { originalEventUuid = event.uuid
                      , action = ApplyKnowledgeModelMigrationAction
                      }
              nextState <- migrate (solveConflict migratorState conflictDto)
              go nextState
            Nothing -> do
              let updatedMigratorState = migratorState {state = ErrorKnowledgeModelMigrationState} :: KnowledgeModelMigration
              updateMigratorState updatedMigratorState
              return updatedMigratorState
        ErrorKnowledgeModelMigrationState -> return migratorState
        CompletedKnowledgeModelMigrationState -> return migratorState

migrateState :: KnowledgeModelMigration -> AppContextM KnowledgeModelMigration
migrateState ms =
  runInTransaction $ do
    migratedMs <- migrate ms
    updateMigratorState migratedMs
    return migratedMs
