module Main where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec

import LensesConfig
import Wizard.Constant.Resource
import Wizard.Database.Connection
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Integration.Http.Common.HttpClientFactory
import Wizard.Messaging.Connection
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.ApplicationConfigService
import Wizard.Service.Config.BuildInfoConfigService
import Wizard.Service.User.UserMapper

import Wizard.Specs.API.BookReference.APISpec
import Wizard.Specs.API.Branch.APISpec
import Wizard.Specs.API.Config.APISpec
import Wizard.Specs.API.Feedback.APISpec
import Wizard.Specs.API.Info.APISpec
import Wizard.Specs.API.KnowledgeModel.APISpec
import Wizard.Specs.API.Level.APISpec
import Wizard.Specs.API.Metric.APISpec
import Wizard.Specs.API.MigrationAPISpec
import Wizard.Specs.API.Organization.APISpec
import Wizard.Specs.API.Package.APISpec
import Wizard.Specs.API.Questionnaire.APISpec
import Wizard.Specs.API.Questionnaire.Migration.APISpec
import Wizard.Specs.API.Template.APISpec
import Wizard.Specs.API.Token.APISpec
import Wizard.Specs.API.Typehint.APISpec
import Wizard.Specs.API.UserAPISpec
import Wizard.Specs.API.Version.APISpec
import Wizard.Specs.Integration.Http.Common.ResponseMapperSpec
import Wizard.Specs.Integration.Http.Typehint.ResponseMapperSpec
import Wizard.Specs.Localization.LocaleSpec
import Wizard.Specs.Service.Branch.BranchServiceSpec
import Wizard.Specs.Service.Branch.BranchValidationSpec
import Wizard.Specs.Service.Document.DocumentServiceSpec
import Wizard.Specs.Service.Feedback.FeedbackServiceSpec
import Wizard.Specs.Service.KnowledgeModel.Compilator.CompilatorSpec
import Wizard.Specs.Service.KnowledgeModel.Compilator.Modifier.ModifierSpec
import Wizard.Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec
import Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.MigrationSpec
import qualified Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.SanitizatorSpec as KM_SanitizatorSpec
import qualified Wizard.Specs.Service.Migration.Questionnaire.ChangeQTypeSanitizatorSpec as QTN_ChangeQTypeSanitizator
import qualified Wizard.Specs.Service.Migration.Questionnaire.MoveSanitizatorSpec as QTN_MoveSanitizatorSpec
import Wizard.Specs.Service.Organization.OrganizationValidationSpec
import Wizard.Specs.Service.Package.PackageValidationSpec
import Wizard.Specs.Service.PublicQuestionnaire.PublicQuestionnaireServiceSpec
import Wizard.Specs.Service.Report.ReportGeneratorSpec
import Wizard.Specs.Service.Template.TemplateServiceSpec
import Wizard.Specs.Service.Token.TokenServiceSpec
import Wizard.Specs.Service.User.UserServiceSpec
import Wizard.Specs.Util.ListSpec
import Wizard.TestMigration

hLoadConfig fileName loadFn callback = do
  eitherConfig <- loadFn fileName
  case eitherConfig of
    Left error -> do
      putStrLn $ "CONFIG: load failed (" ++ fileName ++ ")"
      putStrLn $ "CONFIG: can't load " ++ fileName ++ ". Maybe the file is missing or not well-formatted"
      putStrLn $ "CONFIG: " ++ show error
    Right config -> do
      putStrLn $ "CONFIG: '" ++ fileName ++ "' loaded"
      callback config

prepareWebApp runCallback =
  hLoadConfig applicationConfigFileTest getApplicationConfig $ \appConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      putStrLn $ "ENVIRONMENT: set to " `mappend` show (appConfig ^. general . environment)
      dbPool <- createDatabaseConnectionPool appConfig
      putStrLn "DATABASE: connected"
      msgChannel <- createMessagingChannel appConfig
      putStrLn "MESSAGING: connected"
      httpClientManager <- createHttpClientManager appConfig
      putStrLn "HTTP_CLIENT: created"
      let appContext =
            AppContext
              { _appContextApplicationConfig = appConfig
              , _appContextLocalization = M.empty
              , _appContextBuildInfoConfig = buildInfoConfig
              , _appContextPool = dbPool
              , _appContextMsgChannel = msgChannel
              , _appContextHttpClientManager = httpClientManager
              , _appContextTraceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
              , _appContextCurrentUser = Just . toDTO $ userAlbert
              }
      runCallback appContext

main :: IO ()
main =
  prepareWebApp
    (\appContext ->
       hspec $ do
         describe "UNIT TESTING" $ do
           describe "INTEGRATION" $ describe "Http" $ do
             describe "Common" commonResponseMapperSpec
             describe "Typehint" typehintResponseMapperSpec
           describe "LOCALIZATION" localeSpec
           describe "SERVICE" $ do
             describe "Branch" branchValidationSpec
             describe "KnowledgeModel" $ do
               describe "Compilator" $ do
                 describe "Modifier" modifierSpec
                 compilatorSpec
               knowledgeModelFilterSpec
             describe "Migration" $ do
               describe "KnowledgeModel" $ describe "Migrator" $ do
                 migratorSpec
                 KM_SanitizatorSpec.sanitizatorSpec
               describe "Questionnaire" $ describe "Migrator" $ do
                 QTN_ChangeQTypeSanitizator.sanitizatorSpec
                 QTN_MoveSanitizatorSpec.sanitizatorSpec
             describe "Organization" organizationValidationSpec
             describe "Package" packageValidationSpec
             describe "Report" reportGeneratorSpec
             describe "Template" templateServiceSpec
             describe "Token" tokenServiceSpec
           describe "UTIL" listSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ do
           describe "API" $ do
             bookReferenceAPI appContext
             branchAPI appContext
             configAPI appContext
             feedbackAPI appContext
             infoAPI appContext
             knowledgeModelAPI appContext
             levelAPI appContext
             metricAPI appContext
             migratorAPI appContext
             organizationAPI appContext
             packageAPI appContext
             questionnaireAPI appContext
             questionnaireMigrationAPI appContext
             templateAPI appContext
             typehintAPI appContext
             tokenAPI appContext
             userAPI appContext
             versionAPI appContext
           describe "SERVICE" $ do
             branchServiceIntegrationSpec appContext
             feedbackServiceIntegrationSpec appContext
             documentIntegrationSpec appContext
             publicQuestionnaireServiceIntegrationSpec appContext
             userServiceIntegrationSpec appContext)
