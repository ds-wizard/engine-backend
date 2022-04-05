module Main where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec

import LensesConfig
import Shared.Constant.App
import Shared.Database.Connection
import Shared.Integration.Http.Common.HttpClientFactory
import Shared.S3.Common
import Shared.Service.Config.BuildInfoConfigService
import Wizard.Bootstrap.ServerCache
import Wizard.Constant.Resource
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.ServerConfigService
import Wizard.Service.User.UserMapper

import Wizard.Specs.API.App.APISpec
import Wizard.Specs.API.App.Plan.APISpec
import Wizard.Specs.API.BookReference.APISpec
import Wizard.Specs.API.Branch.APISpec
import Wizard.Specs.API.Config.APISpec
import Wizard.Specs.API.Document.APISpec
import Wizard.Specs.API.Domain.APISpec
import Wizard.Specs.API.Feedback.APISpec
import Wizard.Specs.API.Info.APISpec
import Wizard.Specs.API.KnowledgeModel.APISpec
import qualified Wizard.Specs.API.Migration.KnowledgeModel.APISpec as KM_MigrationAPI
import qualified Wizard.Specs.API.Migration.Questionnaire.APISpec as QTN_MigrationAPI
import Wizard.Specs.API.Package.APISpec
import Wizard.Specs.API.Prefab.APISpec
import Wizard.Specs.API.Questionnaire.APISpec
import Wizard.Specs.API.Questionnaire.ProjectTag.APISpec
import Wizard.Specs.API.Questionnaire.Version.APISpec
import Wizard.Specs.API.Submission.APISpec
import Wizard.Specs.API.Swagger.APISpec
import Wizard.Specs.API.Template.APISpec
import Wizard.Specs.API.Template.Asset.APISpec
import Wizard.Specs.API.Template.File.APISpec
import Wizard.Specs.API.Token.APISpec
import Wizard.Specs.API.Typehint.APISpec
import Wizard.Specs.API.Usage.APISpec
import Wizard.Specs.API.User.APISpec
import Wizard.Specs.API.Version.APISpec
import Wizard.Specs.Integration.Http.Common.ResponseMapperSpec
import Wizard.Specs.Integration.Http.Typehint.ResponseMapperSpec
import Wizard.Specs.Service.App.AppValidationSpec
import Wizard.Specs.Service.Branch.BranchServiceSpec
import Wizard.Specs.Service.Branch.BranchValidationSpec
import Wizard.Specs.Service.Config.AppConfigValidationSpec
import Wizard.Specs.Service.Coordinate.CoordinateValidationSpec
import Wizard.Specs.Service.Document.DocumentServiceSpec
import Wizard.Specs.Service.Feedback.FeedbackServiceSpec
import Wizard.Specs.Service.KnowledgeModel.Compilator.CompilatorSpec
import Wizard.Specs.Service.KnowledgeModel.Compilator.Modifier.ModifierSpec
import Wizard.Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec
import Wizard.Specs.Service.KnowledgeModel.Squash.SquasherSpec
import Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.MigrationSpec
import qualified Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.SanitizatorSpec as KM_SanitizatorSpec
import qualified Wizard.Specs.Service.Migration.Questionnaire.ChangeQTypeSanitizatorSpec as QTN_ChangeQTypeSanitizator
import qualified Wizard.Specs.Service.Migration.Questionnaire.MoveSanitizatorSpec as QTN_MoveSanitizatorSpec
import qualified Wizard.Specs.Service.Migration.Questionnaire.SanitizatorSpec as QTN_SanitizatorSpec
import Wizard.Specs.Service.Package.PackageUtilSpec
import Wizard.Specs.Service.Package.PackageValidationSpec
import Wizard.Specs.Service.Questionnaire.Collaboration.CollaborationAclSpec
import Wizard.Specs.Service.Questionnaire.Compiler.CompilerServiceSpec
import Wizard.Specs.Service.Questionnaire.Event.QuestionnaireEventServiceSpec
import Wizard.Specs.Service.Questionnaire.QuestionnaireAclSpec
import Wizard.Specs.Service.Questionnaire.QuestionnaireServiceSpec
import Wizard.Specs.Service.Questionnaire.QuestionnaireValidationSpec
import Wizard.Specs.Service.Report.ReportGeneratorSpec
import Wizard.Specs.Service.Template.TemplateUtilSpec
import Wizard.Specs.Service.Token.TokenServiceSpec
import Wizard.Specs.Service.User.UserServiceSpec
import Wizard.Specs.Websocket.Branch.Detail.WebsocketSpec
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Questionnaire.Detail.WebsocketSpec
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
  hLoadConfig serverConfigFileTest getServerConfig $ \serverConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      shutdownFlag <- newEmptyMVar
      putStrLn $ "ENVIRONMENT: set to " `mappend` show (serverConfig ^. general . environment)
      dbPool <- createDatabaseConnectionPool (serverConfig ^. database)
      putStrLn "DATABASE: connected"
      httpClientManager <- createHttpClientManager (serverConfig ^. logging)
      putStrLn "HTTP_CLIENT: created"
      s3Client <- createS3Client (serverConfig ^. s3) httpClientManager
      putStrLn "S3_CLIENT: created"
      registryClient <- createRegistryClient serverConfig httpClientManager
      putStrLn "REGISTRY_CLIENT: created"
      cache <- createServerCache
      putStrLn "CACHE: created"
      let appContext =
            AppContext
              { _appContextServerConfig = serverConfig
              , _appContextLocalization = M.empty
              , _appContextBuildInfoConfig = buildInfoConfig
              , _appContextDbPool = dbPool
              , _appContextS3Client = s3Client
              , _appContextHttpClientManager = httpClientManager
              , _appContextRegistryClient = registryClient
              , _appContextTraceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
              , _appContextAppUuid = defaultAppUuid
              , _appContextCurrentUser = Just . toDTO $ userAlbert
              , _appContextShutdownFlag = shutdownFlag
              , _appContextCache = cache
              }
      putStrLn "DB: start creating schema"
      buildSchema appContext
      putStrLn "DB: schema created"
      runWebserver appContext (runCallback appContext)

main :: IO ()
main =
  prepareWebApp
    (\appContext ->
       hspec $ do
         describe "UNIT TESTING" $ do
           describe "INTEGRATION" $ describe "Http" $ do
             describe "Common" commonResponseMapperSpec
             describe "Typehint" typehintResponseMapperSpec
           describe "SERVICE" $ do
             describe "App" appValidationSpec
             describe "Branch" branchValidationSpec
             describe "Config" appConfigValidationSpec
             describe "KnowledgeModel" $ do
               describe "Compilator" $ do
                 describe "Modifier" modifierSpec
                 compilatorSpec
               describe "Squash" $ do squasherSpec
               knowledgeModelFilterSpec
             describe "Migration" $ do
               describe "KnowledgeModel" $ describe "Migrator" $ do
                 migratorSpec
                 KM_SanitizatorSpec.sanitizatorSpec
               describe "Questionnaire" $ describe "Migrator" $ do
                 QTN_ChangeQTypeSanitizator.sanitizatorSpec
                 QTN_MoveSanitizatorSpec.sanitizatorSpec
             describe "Package" packageUtilSpec
             describe "Questionnaire" $ do
               describe "Event" questionnaireEventServiceSpec
               questionnaireValidationSpec
             describe "Report" reportGeneratorSpec
             describe "Token" tokenServiceSpec
             describe "Template" templateUtilSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ do
           describe "API" $ do
             appAPI appContext
             appPlanAPI appContext
             bookReferenceAPI appContext
             branchAPI appContext
             configAPI appContext
             documentAPI appContext
             domainAPI appContext
             feedbackAPI appContext
             infoAPI appContext
             knowledgeModelAPI appContext
             KM_MigrationAPI.migrationAPI appContext
             QTN_MigrationAPI.migrationAPI appContext
             packageAPI appContext
             prefabAPI appContext
             questionnaireAPI appContext
             questionnaireProjectTagAPI appContext
             questionnaireVersionAPI appContext
             submissionAPI appContext
             swaggerAPI appContext
             templateAPI appContext
             templateAssetAPI appContext
             templateFileAPI appContext
             typehintAPI appContext
             tokenAPI appContext
             usageAPI appContext
             userAPI appContext
             versionAPI appContext
           describe "SERVICE" $ do
             branchServiceIntegrationSpec appContext
             coordinateValidationSpec appContext
             feedbackServiceIntegrationSpec appContext
             documentIntegrationSpec appContext
             describe "Migration" $ describe "Questionnaire" $ describe "Migrator" $
               QTN_SanitizatorSpec.sanitizatorIntegrationSpec appContext
             packageValidationSpec appContext
             describe "Questionnaire" $ do
               questionnaireCollaborationAclSpec appContext
               questionnaireCompilerServiceSpec appContext
               questionnaireAclSpec appContext
               questionnaireServiceSpec appContext
             userServiceIntegrationSpec appContext
           describe "WEBSOCKET" $ do
             branchesWebsocketAPI appContext
             questionnaireWebsocketAPI appContext)
