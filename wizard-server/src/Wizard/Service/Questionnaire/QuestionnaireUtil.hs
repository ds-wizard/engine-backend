module Wizard.Service.Questionnaire.QuestionnaireUtil where

import Control.Monad (when)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Common.Lens
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Database.DAO.Questionnaire.MigratorDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Database.DAO.User.UserGroupDAO

extractVisibility dto = do
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  if tcQuestionnaire.questionnaireVisibility.enabled
    then return dto.visibility
    else return $ tcQuestionnaire.questionnaireVisibility.defaultValue

extractSharing dto = do
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  if tcQuestionnaire.questionnaireSharing.enabled
    then return dto.sharing
    else return $ tcQuestionnaire.questionnaireSharing.defaultValue

enhanceQuestionnairePerm :: QuestionnairePerm -> AppContextM QuestionnairePermDTO
enhanceQuestionnairePerm qtnPerm =
  case qtnPerm.memberType of
    UserQuestionnairePermType -> do
      user <- findUserByUuid qtnPerm.memberUuid
      return $ toUserQuestionnairePermDTO qtnPerm user
    UserGroupQuestionnairePermType -> do
      userGroup <- findUserGroupByUuid qtnPerm.memberUuid
      return $ toUserGroupQuestionnairePermDTO qtnPerm userGroup

enhanceQuestionnaireEvent :: QuestionnaireEvent -> AppContextM QuestionnaireEventDTO
enhanceQuestionnaireEvent event = do
  mUser <-
    case getCreatedBy event of
      Just userUuid -> findUserByUuid' userUuid
      Nothing -> return Nothing
  return $ toEventDTO event mUser

getQuestionnaireState :: U.UUID -> String -> AppContextM QuestionnaireState
getQuestionnaireState qtnUuid pkgId = do
  mMs <- findMigratorStateByNewQuestionnaireUuid' qtnUuid
  case mMs of
    Just _ -> return QSMigrating
    Nothing -> do
      pkgs <- getNewerPackages pkgId True
      if null pkgs
        then return QSDefault
        else return QSOutdated

getQuestionnaireReport
  :: ( HasField "uuid" questionnaire U.UUID
     , HasField "knowledgeModelPackageId" questionnaire String
     , HasField "selectedQuestionTagUuids" questionnaire [U.UUID]
     )
  => questionnaire
  -> [QuestionnaireEvent]
  -> AppContextM QuestionnaireReportDTO
getQuestionnaireReport qtn events = do
  qtnCtn <- compileQuestionnaire events
  let _requiredPhaseUuid = qtnCtn.phaseUuid
  km <- compileKnowledgeModel [] (Just qtn.knowledgeModelPackageId) qtn.selectedQuestionTagUuids
  let indications = computeTotalReportIndications _requiredPhaseUuid km qtnCtn.replies
  return . toQuestionnaireReportDTO $ indications

getPhasesAnsweredIndication
  :: ( HasField "uuid" questionnaire U.UUID
     , HasField "knowledgeModelPackageId" questionnaire String
     , HasField "selectedQuestionTagUuids" questionnaire [U.UUID]
     )
  => questionnaire
  -> [QuestionnaireEvent]
  -> AppContextM (Maybe PhasesAnsweredIndication)
getPhasesAnsweredIndication qtn events = do
  report <- getQuestionnaireReport qtn events
  return $ foldl go Nothing report.indications
  where
    go acc (PhasesAnsweredIndication' indication) = Just indication
    go acc _ = acc

skipIfAssigningProject :: Questionnaire -> AppContextM () -> AppContextM ()
skipIfAssigningProject qtn action = do
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  let questionnaireSharingEnabled = tcQuestionnaire.questionnaireSharing.enabled
  let questionnaireSharingAnonymousEnabled = tcQuestionnaire.questionnaireSharing.anonymousEnabled
  when
    (not (questionnaireSharingEnabled && questionnaireSharingAnonymousEnabled) || (not . null $ qtn.permissions))
    action
