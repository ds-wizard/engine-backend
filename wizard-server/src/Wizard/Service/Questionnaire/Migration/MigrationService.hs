module Wizard.Service.Questionnaire.Migration.MigrationService where

import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Model.Common.Lens
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.Migration.MigrationAudit
import Wizard.Service.Questionnaire.Migration.MigrationMapper
import Wizard.Service.Questionnaire.Migration.MigrationValidation
import Wizard.Service.Questionnaire.Migration.Migrator.Sanitizer
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireService

createQuestionnaireMigration :: U.UUID -> MigratorStateCreateDTO -> AppContextM MigratorStateDTO
createQuestionnaireMigration oldQtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    validateMigrationExistence oldQtnUuid
    oldQtn <- findQuestionnaireByUuid oldQtnUuid
    checkMigrationPermissionToQtn oldQtn.visibility oldQtn.permissions
    (newQtn, newQtnEvents, newQtnVersions) <- upgradeQuestionnaire reqDto oldQtn
    insertQuestionnaire newQtn
    insertQuestionnaireEvents newQtnEvents
    traverse_ insertQuestionnaireVersion newQtnVersions
    tenantUuid <- asks currentTenantUuid
    let state = fromCreateDTO oldQtn.uuid newQtn.uuid tenantUuid
    insertMigratorState state
    auditQuestionnaireMigrationCreate reqDto oldQtn newQtn
    getQuestionnaireMigration newQtn.uuid

getQuestionnaireMigration :: U.UUID -> AppContextM MigratorStateDTO
getQuestionnaireMigration qtnUuid = do
  checkPermission _QTN_PERM
  state <- findMigratorStateByNewQuestionnaireUuid qtnUuid
  oldQtnDto <- getQuestionnaireDetailQuestionnaireByUuid state.oldQuestionnaireUuid
  newQtnDto <- getQuestionnaireDetailQuestionnaireByUuid state.newQuestionnaireUuid
  oldQtn <- findQuestionnaireByUuid state.oldQuestionnaireUuid
  newQtn <- findQuestionnaireByUuid state.newQuestionnaireUuid
  checkMigrationPermissionToQtn oldQtn.visibility oldQtn.permissions
  checkMigrationPermissionToQtn newQtn.visibility newQtn.permissions
  return $ toDTO oldQtnDto newQtnDto state.resolvedQuestionUuids state.tenantUuid

modifyQuestionnaireMigration :: U.UUID -> MigratorStateChangeDTO -> AppContextM MigratorStateDTO
modifyQuestionnaireMigration qtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    state <- getQuestionnaireMigration qtnUuid
    let updatedState = fromChangeDTO reqDto state
    updateMigratorStateByNewQuestionnaireUuid updatedState
    auditQuestionnaireMigrationModify state reqDto
    return $ toDTO state.oldQuestionnaire state.newQuestionnaire updatedState.resolvedQuestionUuids updatedState.tenantUuid

finishQuestionnaireMigration :: U.UUID -> AppContextM ()
finishQuestionnaireMigration qtnUuid =
  runInTransaction $ do
    checkPermission _QTN_PERM
    _ <- getQuestionnaireMigration qtnUuid
    state <- findMigratorStateByNewQuestionnaireUuid qtnUuid
    deleteMigratorStateByNewQuestionnaireUuid qtnUuid
    oldQtn <- findQuestionnaireByUuid state.oldQuestionnaireUuid
    newQtn <- findQuestionnaireByUuid state.newQuestionnaireUuid
    newQtnEvents <- ensurePhaseIsSetIfNecessary newQtn
    newQtnVersions <- findQuestionnaireVersionsByQuestionnaireUuid state.newQuestionnaireUuid
    now <- liftIO getCurrentTime
    let updatedNewQtn =
          oldQtn
            { formatUuid = newQtn.formatUuid
            , documentTemplateId = newQtn.documentTemplateId
            , selectedQuestionTagUuids = newQtn.selectedQuestionTagUuids
            , knowledgeModelPackageId = newQtn.knowledgeModelPackageId
            , updatedAt = now
            }
          :: Questionnaire
    let newQtnEventsWithOldQtnUuid = fmap (\event -> setQuestionnaireUuid event oldQtn.uuid) newQtnEvents
    newVersionsWithNewUuid <- traverse generateNewVersionUuid newQtnVersions
    let newVersionsWithOldQtnUuid = fmap (\v -> v {questionnaireUuid = oldQtn.uuid} :: QuestionnaireVersion) newVersionsWithNewUuid
    -- Delete the new questionnaire
    deleteQuestionnaireEventsByQuestionnaireUuid newQtn.uuid
    deleteQuestionnaire newQtn.uuid False
    -- Update the old questionnaire with values from new questionnaire
    updateQuestionnaireByUuid updatedNewQtn
    deleteQuestionnaireEventsByQuestionnaireUuid oldQtn.uuid
    insertQuestionnaireEvents newQtnEventsWithOldQtnUuid
    traverse_ insertQuestionnaireVersion newVersionsWithOldQtnUuid
    auditQuestionnaireMigrationFinish oldQtn newQtn

cancelQuestionnaireMigration :: U.UUID -> AppContextM ()
cancelQuestionnaireMigration qtnUuid =
  runInTransaction $ do
    checkPermission _QTN_PERM
    state <- getQuestionnaireMigration qtnUuid
    deleteQuestionnaire state.newQuestionnaire.uuid True
    deleteMigratorStateByNewQuestionnaireUuid qtnUuid
    auditQuestionnaireMigrationCancel state
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
upgradeQuestionnaire :: MigratorStateCreateDTO -> Questionnaire -> AppContextM (Questionnaire, [QuestionnaireEvent], [QuestionnaireVersion])
upgradeQuestionnaire reqDto oldQtn = do
  let newPkgId = reqDto.targetKnowledgeModelPackageId
  let newTagUuids = reqDto.targetTagUuids
  oldKm <- compileKnowledgeModel [] (Just oldQtn.knowledgeModelPackageId) newTagUuids
  newKm <- compileKnowledgeModel [] (Just newPkgId) newTagUuids
  newUuid <- liftIO generateUuid
  oldQtnEvents <- findQuestionnaireEventListsByQuestionnaireUuid oldQtn.uuid
  clonedQtnEventsWithOldEventUuid <- cloneQuestionnaireEventsWithOldEventUuid oldQtnEvents
  let clonedQtnEvents = fmap snd clonedQtnEventsWithOldEventUuid
  newQtnEvents <- sanitizeQuestionnaireEvents newUuid oldKm newKm clonedQtnEvents
  (newDocumentTemplateId, newFormatUuid) <- getNewDocumentTemplateIdAndFormatUuid oldQtn newPkgId
  let newQtnEventUuids = fmap getUuid newQtnEvents
  let clonedQtnEventsFiltered = filter (\e -> getUuid (snd e) `elem` newQtnEventUuids) clonedQtnEventsWithOldEventUuid
  let newPermissions = fmap (\perm -> perm {questionnaireUuid = newUuid} :: QuestionnairePerm) oldQtn.permissions
  let upgradedQtn =
        oldQtn
          { uuid = newUuid
          , knowledgeModelPackageId = newPkgId
          , selectedQuestionTagUuids = newTagUuids
          , documentTemplateId = newDocumentTemplateId
          , formatUuid = newFormatUuid
          , permissions = newPermissions
          }
        :: Questionnaire
  versionsWithOldQtnUuid <- findQuestionnaireVersionsByQuestionnaireUuid oldQtn.uuid
  newVersionsWithNewUuid <- traverse generateNewVersionUuid versionsWithOldQtnUuid
  let newVersionsWithNewEventUuid =
        fmap
          ( \v ->
              case L.find (\(oldEventUuid, _) -> v.eventUuid == oldEventUuid) clonedQtnEventsWithOldEventUuid of
                Just (_, newEvent) ->
                  Just $
                    v
                      { questionnaireUuid = newUuid
                      , eventUuid = getUuid newEvent
                      }
                Nothing -> Nothing
          )
          newVersionsWithNewUuid
  let newVersions = catMaybes newVersionsWithNewEventUuid
  return (upgradedQtn, fmap (toEvent upgradedQtn.uuid upgradedQtn.tenantUuid) newQtnEvents, newVersions)

ensurePhaseIsSetIfNecessary :: Questionnaire -> AppContextM [QuestionnaireEvent]
ensurePhaseIsSetIfNecessary newQtn = do
  uuid <- liftIO generateUuid
  mCurrentUser <- asks currentUser
  now <- liftIO getCurrentTime
  newQtnListEvents <- findQuestionnaireEventListsByQuestionnaireUuid newQtn.uuid
  let qtnCtn = compileQuestionnaire newQtnListEvents
  knowledgeModel <- compileKnowledgeModel [] (Just newQtn.knowledgeModelPackageId) newQtn.selectedQuestionTagUuids
  let newQtnEvents = fmap (toEvent newQtn.uuid newQtn.tenantUuid) newQtnListEvents
  return $
    case (headSafe knowledgeModel.phaseUuids, qtnCtn.phaseUuid) of
      (Nothing, Nothing) -> newQtnEvents
      (Nothing, Just qtnPhaseUuid) -> newQtnEvents ++ [toQuestionnairePhaseEvent uuid Nothing newQtn.uuid newQtn.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Nothing) -> newQtnEvents ++ [toQuestionnairePhaseEvent uuid (Just kmPhaseUuid) newQtn.uuid newQtn.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Just qtnPhaseUuid) ->
        if qtnPhaseUuid `notElem` knowledgeModel.phaseUuids
          then newQtnEvents ++ [toQuestionnairePhaseEvent uuid (Just kmPhaseUuid) newQtn.uuid newQtn.tenantUuid mCurrentUser now]
          else newQtnEvents

generateNewVersionUuid :: QuestionnaireVersion -> AppContextM QuestionnaireVersion
generateNewVersionUuid version = do
  newVersionUuid <- liftIO generateUuid
  return $ version {uuid = newVersionUuid}

getNewDocumentTemplateIdAndFormatUuid :: Questionnaire -> String -> AppContextM (Maybe String, Maybe U.UUID)
getNewDocumentTemplateIdAndFormatUuid oldQtn newPkgId = do
  case oldQtn.documentTemplateId of
    Just id -> do
      documentTemplate <- findDocumentTemplateById id
      if isPkgAllowedByDocumentTemplate newPkgId documentTemplate
        then return (Just id, oldQtn.formatUuid)
        else return (Nothing, Nothing)
    Nothing -> return (Nothing, Nothing)
