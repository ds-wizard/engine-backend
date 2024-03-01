module Wizard.Service.Migration.Questionnaire.MigratorService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator
import Wizard.Service.Migration.Questionnaire.MigratorAudit
import Wizard.Service.Migration.Questionnaire.MigratorMapper
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Questionnaire.QuestionnaireUtil
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

createQuestionnaireMigration :: U.UUID -> MigratorStateCreateDTO -> AppContextM MigratorStateDTO
createQuestionnaireMigration oldQtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    oldQtn <- findQuestionnaireByUuid oldQtnUuid
    checkMigrationPermissionToQtn oldQtn.visibility oldQtn.permissions
    newQtn <- upgradeQuestionnaire reqDto oldQtn
    insertQuestionnaire newQtn
    tenantUuid <- asks currentTenantUuid
    let state = fromCreateDTO oldQtn.uuid newQtn.uuid tenantUuid
    insertMigratorState state
    auditQuestionnaireMigrationCreate reqDto oldQtn newQtn
    getQuestionnaireMigration newQtn.uuid

getQuestionnaireMigration :: U.UUID -> AppContextM MigratorStateDTO
getQuestionnaireMigration qtnUuid = do
  checkPermission _QTN_PERM
  state <- findMigratorStateByNewQuestionnaireUuid qtnUuid
  oldQtnDto <- getQuestionnaireDetailById state.oldQuestionnaireUuid
  newQtnDto <- getQuestionnaireDetailById state.newQuestionnaireUuid
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
    events <- ensurePhaseIsSetIfNecessary newQtn
    now <- liftIO getCurrentTime
    let updatedQtn =
          oldQtn
            { formatUuid = newQtn.formatUuid
            , documentTemplateId = newQtn.documentTemplateId
            , selectedQuestionTagUuids = newQtn.selectedQuestionTagUuids
            , packageId = newQtn.packageId
            , events = events
            , updatedAt = now
            }
          :: Questionnaire
    mPhasesAnsweredIndication <- getPhasesAnsweredIndication updatedQtn
    updateQuestionnaireByUuid updatedQtn
    deleteQuestionnaire newQtn.uuid False
    auditQuestionnaireMigrationFinish oldQtn newQtn
    return ()

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
upgradeQuestionnaire :: MigratorStateCreateDTO -> Questionnaire -> AppContextM Questionnaire
upgradeQuestionnaire reqDto oldQtn = do
  let newPkgId = reqDto.targetPackageId
  let newTagUuids = reqDto.targetTagUuids
  oldKm <- compileKnowledgeModel [] (Just oldQtn.packageId) newTagUuids
  newKm <- compileKnowledgeModel [] (Just newPkgId) newTagUuids
  newUuid <- liftIO generateUuid
  newEvents <- sanitizeQuestionnaireEvents oldKm newKm oldQtn.events
  let newPermissions = fmap (\perm -> perm {questionnaireUuid = newUuid}) oldQtn.permissions
  let upgradedQtn =
        oldQtn
          { uuid = newUuid
          , packageId = newPkgId
          , events = newEvents
          , selectedQuestionTagUuids = newTagUuids
          , documentTemplateId = Nothing
          , formatUuid = Nothing
          , permissions = newPermissions
          }
        :: Questionnaire
  return upgradedQtn

ensurePhaseIsSetIfNecessary :: Questionnaire -> AppContextM [QuestionnaireEvent]
ensurePhaseIsSetIfNecessary newQtn = do
  uuid <- liftIO generateUuid
  mCurrentUser <- asks currentUser
  now <- liftIO getCurrentTime
  qtnCtn <- compileQuestionnaire newQtn
  knowledgeModel <- compileKnowledgeModel [] (Just newQtn.packageId) newQtn.selectedQuestionTagUuids
  let events =
        case (headSafe knowledgeModel.phaseUuids, qtnCtn.phaseUuid) of
          (Nothing, Nothing) -> newQtn.events
          (Nothing, Just qtnPhaseUuid) -> newQtn.events ++ [toPhaseEvent uuid Nothing mCurrentUser now]
          (Just kmPhaseUuid, Nothing) -> newQtn.events ++ [toPhaseEvent uuid (Just kmPhaseUuid) mCurrentUser now]
          (Just kmPhaseUuid, Just qtnPhaseUuid) ->
            if qtnPhaseUuid `notElem` knowledgeModel.phaseUuids
              then newQtn.events ++ [toPhaseEvent uuid (Just kmPhaseUuid) mCurrentUser now]
              else newQtn.events
  return events
