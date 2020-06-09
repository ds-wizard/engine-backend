module Wizard.Service.Questionnaire.QuestionnaireUtils where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Cache.QuestionnaireReportCache
import Wizard.Service.Config.AppConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.User.UserService

extractVisibility dto = do
  appConfig <- getAppConfig
  if appConfig ^. questionnaire . questionnaireVisibility . enabled
    then return (dto ^. visibility)
    else return $ appConfig ^. questionnaire . questionnaireVisibility . defaultValue

enhanceQuestionnaire :: Questionnaire -> AppContextM QuestionnaireDTO
enhanceQuestionnaire qtn = do
  pkg <- findPackageById (qtn ^. packageId)
  state <- getQuestionnaireState (U.toString $ qtn ^. uuid) (pkg ^. pId)
  mOwner <-
    case qtn ^. ownerUuid of
      Just uUuid -> Just <$> getUserById (U.toString uUuid)
      Nothing -> return Nothing
  report <- getQuestionnaireReport qtn
  return $ toDTO qtn pkg state mOwner report

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
      let _replies = qtn ^. replies
      km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
      let indications = computeTotalReportIndications _levelsEnabled _requiredLevel km _replies
      addToCache qtn indications
      return . toQuestionnaireReportDTO $ indications
