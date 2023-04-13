module Wizard.Service.Migration.Questionnaire.MigratorService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.List
import Shared.Util.Uuid
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Acl.AclService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator
import Wizard.Service.Migration.Questionnaire.MigratorAudit
import Wizard.Service.Migration.Questionnaire.MigratorMapper
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireService

createQuestionnaireMigration :: U.UUID -> MigratorStateCreateDTO -> AppContextM MigratorStateDTO
createQuestionnaireMigration oldQtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    oldQtn <- findQuestionnaireByUuid oldQtnUuid
    checkMigrationPermissionToQtn oldQtn.visibility oldQtn.permissions
    newQtn <- upgradeQuestionnaire reqDto oldQtn
    insertQuestionnaire newQtn
    appUuid <- asks currentAppUuid
    let state = fromCreateDTO oldQtn.uuid newQtn.uuid appUuid
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
  return $ toDTO oldQtnDto newQtnDto state.resolvedQuestionUuids state.appUuid

modifyQuestionnaireMigration :: U.UUID -> MigratorStateChangeDTO -> AppContextM MigratorStateDTO
modifyQuestionnaireMigration qtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    state <- getQuestionnaireMigration qtnUuid
    let updatedState = fromChangeDTO reqDto state
    updateMigratorStateByNewQuestionnaireUuid updatedState
    auditQuestionnaireMigrationModify state reqDto
    return $ toDTO state.oldQuestionnaire state.newQuestionnaire updatedState.resolvedQuestionUuids updatedState.appUuid

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
    let updatedQtn =
          oldQtn
            { formatUuid = newQtn.formatUuid
            , documentTemplateId = newQtn.documentTemplateId
            , selectedQuestionTagUuids = newQtn.selectedQuestionTagUuids
            , packageId = newQtn.packageId
            , events = events
            }
          :: Questionnaire
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
  newPermissions <- traverse (upgradeQuestionnairePerm newUuid) oldQtn.permissions
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

upgradeQuestionnairePerm :: U.UUID -> QuestionnairePermRecord -> AppContextM QuestionnairePermRecord
upgradeQuestionnairePerm newQtnUuid perm = do
  newUuid <- liftIO generateUuid
  let upgradedPerm =
        perm
          { uuid = newUuid
          , questionnaireUuid = newQtnUuid
          }
  return upgradedPerm

ensurePhaseIsSetIfNecessary :: Questionnaire -> AppContextM [QuestionnaireEvent]
ensurePhaseIsSetIfNecessary newQtn = do
  uuid <- liftIO generateUuid
  mCurrentUser <- asks currentUser
  now <- liftIO getCurrentTime
  qtnCtn <- compileQuestionnaire newQtn
  knowledgeModel <- compileKnowledgeModel [] (Just newQtn.packageId) newQtn.selectedQuestionTagUuids
  let events =
        case (qtnCtn.phaseUuid, headSafe knowledgeModel.phaseUuids) of
          (Nothing, Just kmPhaseUuid) -> newQtn.events ++ [toPhaseEvent uuid kmPhaseUuid mCurrentUser now]
          _ -> newQtn.events
  return events
