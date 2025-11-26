module Wizard.Service.Questionnaire.QuestionnaireUtil where

import Control.Monad (when)
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Database.DAO.Questionnaire.MigratorDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.Questionnaire.QuestionnaireMapper
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

skipIfAssigningProject :: Questionnaire -> AppContextM () -> AppContextM ()
skipIfAssigningProject qtn action = do
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  let questionnaireSharingEnabled = tcQuestionnaire.questionnaireSharing.enabled
  let questionnaireSharingAnonymousEnabled = tcQuestionnaire.questionnaireSharing.anonymousEnabled
  when
    (not (questionnaireSharingEnabled && questionnaireSharingAnonymousEnabled) || (not . null $ qtn.permissions))
    action
