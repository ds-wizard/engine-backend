module Wizard.Service.Questionnaire.QuestionnaireUtils where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Database.DAO.Acl.GroupDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Cache.QuestionnaireReportCache
import Wizard.Service.Config.AppConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Report.ReportGenerator

extractVisibility dto = do
  appConfig <- getAppConfig
  if appConfig ^. questionnaire . questionnaireVisibility . enabled
    then return (dto ^. visibility)
    else return $ appConfig ^. questionnaire . questionnaireVisibility . defaultValue

extractSharing dto = do
  appConfig <- getAppConfig
  if appConfig ^. questionnaire . questionnaireSharing . enabled
    then return (dto ^. sharing)
    else return $ appConfig ^. questionnaire . questionnaireSharing . defaultValue

enhanceQuestionnaire :: Questionnaire -> AppContextM QuestionnaireDTO
enhanceQuestionnaire qtn = do
  pkg <- getPackageById (qtn ^. packageId)
  state <- getQuestionnaireState (U.toString $ qtn ^. uuid) (pkg ^. pId)
  report <- getQuestionnaireReport qtn
  permissionDtos <- traverse enhanceQuestionnairePermRecord (qtn ^. permissions)
  return $ toDTO qtn pkg state report permissionDtos

enhanceQuestionnairePermRecord :: QuestionnairePermRecord -> AppContextM QuestionnairePermRecordDTO
enhanceQuestionnairePermRecord record =
  case record ^. member of
    UserMember {_userMemberUuid = userUuid} -> do
      user <- findUserById (U.toString userUuid)
      return $ toUserPermRecordDTO record user
    GroupMember {_groupMemberGId = groupId} -> do
      group <- findGroupById groupId
      return $ toGroupPermRecordDTO record group

getQuestionnaireState :: String -> String -> AppContextM QuestionnaireState
getQuestionnaireState qtnUuid pkgId = do
  mMs <- findMigratorStateByNewQuestionnaireId' qtnUuid
  case mMs of
    Just _ -> return QSMigrating
    Nothing -> do
      pkgs <- getNewerPackages pkgId
      if null pkgs
        then return QSDefault
        else return QSOutdated

getQuestionnaireReport :: Questionnaire -> AppContextM QuestionnaireReportDTO
getQuestionnaireReport qtn = do
  mIndications <- getFromCache qtn
  case mIndications of
    Just indications -> return . toQuestionnaireReportDTO $ indications
    Nothing -> do
      appConfig <- getAppConfig
      let _levelsEnabled = appConfig ^. questionnaire . levels . enabled
      let _requiredLevel = qtn ^. level
      let _replies = M.toList $ qtn ^. replies
      km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
      let indications = computeTotalReportIndications _levelsEnabled _requiredLevel km _replies
      addToCache qtn indications
      return . toQuestionnaireReportDTO $ indications
