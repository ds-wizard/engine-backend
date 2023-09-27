module Wizard.Service.Questionnaire.QuestionnaireUtil where

import Control.Monad (when)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Common.Lens
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Database.DAO.Acl.GroupDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.KnowledgeModel.Model.Package.Package

extractVisibility dto = do
  tenantConfig <- getCurrentTenantConfig
  if tenantConfig.questionnaire.questionnaireVisibility.enabled
    then return dto.visibility
    else return $ tenantConfig.questionnaire.questionnaireVisibility.defaultValue

extractSharing dto = do
  tenantConfig <- getCurrentTenantConfig
  if tenantConfig.questionnaire.questionnaireSharing.enabled
    then return dto.sharing
    else return $ tenantConfig.questionnaire.questionnaireSharing.defaultValue

enhanceQuestionnaire :: Questionnaire -> AppContextM QuestionnaireDTO
enhanceQuestionnaire qtn = do
  pkg <- getPackageById qtn.packageId
  state <- getQuestionnaireState qtn.uuid pkg.pId
  permissionDtos <- traverse enhanceQuestionnairePermRecord qtn.permissions
  return $ toDTO qtn pkg state permissionDtos

enhanceQuestionnairePermRecord :: QuestionnairePermRecord -> AppContextM QuestionnairePermRecordDTO
enhanceQuestionnairePermRecord record =
  case record.member of
    UserMember {uuid = userUuid} -> do
      user <- findUserByUuid userUuid
      return $ toUserPermRecordDTO record user
    GroupMember {gId = groupId} -> do
      group <- findGroupById groupId
      return $ toGroupPermRecordDTO record group

enhanceQuestionnaireEvent :: QuestionnaireEvent -> AppContextM QuestionnaireEventDTO
enhanceQuestionnaireEvent event = do
  mUser <-
    case getCreatedBy event of
      Just userUuid -> findUserByUuid' userUuid
      Nothing -> return Nothing
  return $ toEventDTO event mUser

enhanceQuestionnaireVersion :: QuestionnaireVersion -> AppContextM QuestionnaireVersionDTO
enhanceQuestionnaireVersion version = do
  mUser <- findUserByUuid' version.createdBy
  return $ toVersionDTO version mUser

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
  :: ( HasField "events" questionnaire [QuestionnaireEvent]
     , HasField "uuid" questionnaire U.UUID
     , HasField "packageId" questionnaire String
     , HasField "selectedQuestionTagUuids" questionnaire [U.UUID]
     )
  => questionnaire
  -> AppContextM QuestionnaireReportDTO
getQuestionnaireReport qtn = do
  qtnCtn <- compileQuestionnaire qtn
  let _requiredPhaseUuid = qtnCtn.phaseUuid
  let _replies = M.toList $ qtnCtn.replies
  km <- compileKnowledgeModel [] (Just qtn.packageId) qtn.selectedQuestionTagUuids
  let indications = computeTotalReportIndications _requiredPhaseUuid km _replies
  qtnCtn <- compileQuestionnaire qtn
  return . toQuestionnaireReportDTO $ indications

getPhasesAnsweredIndication
  :: ( HasField "events" questionnaire [QuestionnaireEvent]
     , HasField "uuid" questionnaire U.UUID
     , HasField "packageId" questionnaire String
     , HasField "selectedQuestionTagUuids" questionnaire [U.UUID]
     )
  => questionnaire
  -> AppContextM (Maybe PhasesAnsweredIndication)
getPhasesAnsweredIndication qtn = do
  report <- getQuestionnaireReport qtn
  return $ foldl go Nothing report.indications
  where
    go acc (PhasesAnsweredIndication' indication) = Just indication
    go acc _ = acc

skipIfAssigningProject :: Questionnaire -> AppContextM () -> AppContextM ()
skipIfAssigningProject qtn action = do
  tenantConfig <- getCurrentTenantConfig
  let questionnaireSharingEnabled = tenantConfig.questionnaire.questionnaireSharing.enabled
  let questionnaireSharingAnonymousEnabled = tenantConfig.questionnaire.questionnaireSharing.anonymousEnabled
  when
    (not (questionnaireSharingEnabled && questionnaireSharingAnonymousEnabled) || (not . null $ qtn.permissions))
    action
