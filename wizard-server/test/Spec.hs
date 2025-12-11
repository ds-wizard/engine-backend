module Main where

import Control.Concurrent.MVar
import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.UUID as U
import Test.Hspec

import Shared.Common.Constant.Tenant
import Shared.Common.Database.Connection
import Shared.Common.Integration.Http.Common.HttpClientFactory
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.S3.Common
import Shared.Common.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Common.Service.Config.Server.ServerConfigService
import Wizard.Cache.CacheFactory
import Wizard.Constant.Resource
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.Server.ServerConfigValidation
import Wizard.Service.User.UserMapper

import Wizard.Specs.API.ApiKey.APISpec
import Wizard.Specs.API.AppKey.APISpec
import Wizard.Specs.API.Config.APISpec
import Wizard.Specs.API.Document.APISpec
import Wizard.Specs.API.DocumentTemplate.APISpec
import Wizard.Specs.API.DocumentTemplateDraft.APISpec
import Wizard.Specs.API.DocumentTemplateDraft.Asset.APISpec
import Wizard.Specs.API.DocumentTemplateDraft.File.APISpec
import Wizard.Specs.API.DocumentTemplateDraft.Folder.APISpec
import Wizard.Specs.API.Domain.APISpec
import Wizard.Specs.API.ExternalLink.APISpec
import Wizard.Specs.API.Feedback.APISpec
import Wizard.Specs.API.Info.APISpec
import Wizard.Specs.API.KnowledgeModel.APISpec
import Wizard.Specs.API.KnowledgeModelEditor.APISpec
import Wizard.Specs.API.KnowledgeModelPackage.APISpec
import Wizard.Specs.API.KnowledgeModelSecret.APISpec
import Wizard.Specs.API.Locale.APISpec
import Wizard.Specs.API.Prefab.APISpec
import Wizard.Specs.API.Project.APISpec
import Wizard.Specs.API.ProjectAction.APISpec
import Wizard.Specs.API.ProjectCommentThread.APISpec
import Wizard.Specs.API.ProjectImporter.APISpec
import Wizard.Specs.API.Submission.APISpec
import Wizard.Specs.API.Swagger.APISpec
import Wizard.Specs.API.Tenant.APISpec
import Wizard.Specs.API.Tenant.Config.APISpec
import Wizard.Specs.API.Tenant.Limit.APISpec
import Wizard.Specs.API.Tenant.Usage.APISpec
import Wizard.Specs.API.Token.APISpec
import Wizard.Specs.API.TypeHint.APISpec
import Wizard.Specs.API.User.APISpec
import Wizard.Specs.API.User.News.APISpec
import Wizard.Specs.API.User.Tour.APISpec
import Wizard.Specs.API.UserGroup.APISpec
import Wizard.Specs.Integration.Http.TypeHint.ResponseMapperSpec
import Wizard.Specs.Service.Document.DocumentServiceSpec
import Wizard.Specs.Service.DocumentTemplate.DocumentTemplateUtilSpec
import Wizard.Specs.Service.KnowledgeModel.Compiler.CompilerSpec
import Wizard.Specs.Service.KnowledgeModel.Compiler.Modifier.ModifierSpec
import Wizard.Specs.Service.KnowledgeModel.Editor.KnowledgeModelEditorServiceSpec
import Wizard.Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec
import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.EventMigratorSpec
import Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.MigrationSpec
import qualified Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.SanitizerSpec as KM_SanitizerSpec
import Wizard.Specs.Service.KnowledgeModel.Package.PackageUtilSpec
import Wizard.Specs.Service.KnowledgeModel.Package.PackageValidationSpec
import Wizard.Specs.Service.KnowledgeModel.Squash.SquasherSpec
import Wizard.Specs.Service.Project.Collaboration.ProjectCollaborationAclSpec
import Wizard.Specs.Service.Project.Compiler.ProjectCompilerServiceSpec
import Wizard.Specs.Service.Project.Event.ProjectEventServiceSpec
import qualified Wizard.Specs.Service.Project.Migration.Migrator.ChangeQTypeSanitizerSpec as PRJ_ChangeQTypeSanitizer
import qualified Wizard.Specs.Service.Project.Migration.Migrator.MoveSanitizerSpec as PRJ_MoveSanitizerSpec
import qualified Wizard.Specs.Service.Project.Migration.Migrator.SanitizerSpec as PRJ_SanitizerSpec
import Wizard.Specs.Service.Project.ProjectAclSpec
import Wizard.Specs.Service.Project.ProjectServiceSpec
import Wizard.Specs.Service.Project.ProjectValidationSpec
import Wizard.Specs.Service.Report.ReportGeneratorSpec
import Wizard.Specs.Service.Tenant.Config.TenantConfigValidationSpec
import Wizard.Specs.Service.Tenant.TenantValidationSpec
import Wizard.Specs.Service.User.UserServiceSpec
import Wizard.Specs.Util.JinjaSpec
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.WebsocketSpec
import Wizard.Specs.Websocket.Project.Detail.WebsocketSpec
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
  hLoadConfig serverConfigFileTest (getServerConfig validateServerConfig) $ \serverConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      shutdownFlag <- newEmptyMVar
      putStrLn $ "ENVIRONMENT: set to " `mappend` serverConfig.general.environment
      dbPool <- createDatabaseConnectionPool serverConfig.database
      putStrLn "DATABASE: connected"
      httpClientManager <- createHttpClientManager serverConfig.logging
      putStrLn "HTTP_CLIENT: created"
      s3Client <- createS3Client serverConfig.s3 httpClientManager
      putStrLn "S3_CLIENT: created"
      registryClient <- createRegistryClient serverConfig httpClientManager
      putStrLn "REGISTRY_CLIENT: created"
      cache <- createServerCache serverConfig
      putStrLn "CACHE: created"
      let baseContext =
            BaseContext
              { serverConfig = serverConfig
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              , httpClientManager = httpClientManager
              , registryClient = registryClient
              , shutdownFlag = shutdownFlag
              , cache = cache
              }
      withResource dbPool $ \dbConnection -> do
        let appContext =
              AppContext
                { serverConfig = serverConfig
                , buildInfoConfig = buildInfoConfig
                , dbPool = dbPool
                , dbConnection = Just dbConnection
                , s3Client = s3Client
                , httpClientManager = httpClientManager
                , registryClient = registryClient
                , traceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
                , currentTenantUuid = defaultTenantUuid
                , currentUser = Just . toDTO $ userAlbert
                , shutdownFlag = shutdownFlag
                , cache = cache
                }
        putStrLn "DB: start creating schema"
        buildSchema appContext
        putStrLn "DB: schema created"
        runWebserver appContext (runCallback baseContext appContext)

main :: IO ()
main =
  prepareWebApp
    ( \baseContext appContext ->
        hspec $ do
          describe "UNIT TESTING" $ do
            describe "INTEGRATION" $
              describe "Http" $
                describe "TypeHint" typeHintResponseMapperSpec
            describe "SERVICE" $ do
              describe "Document Template" documentTemplateUtilSpec
              describe "KnowledgeModel" $ do
                describe "Metamodel" $
                  describe
                    "Migration"
                    eventMigratorSpec
                describe "Compiler" $ do
                  describe "Modifier" modifierSpec
                  compilerSpec
                describe "Package" packageUtilSpec
                describe "Squash" $ do squasherSpec
                knowledgeModelFilterSpec
              describe "Migration" $ do
                describe "Project" $ describe "Migration" $ do
                  PRJ_ChangeQTypeSanitizer.sanitizerSpec
                  PRJ_MoveSanitizerSpec.sanitizerSpec
              describe "Project" $ do
                describe "Event" $ do
                  projectCompilerServiceSpec
                  projectEventServiceSpec
                projectValidationSpec
              describe "Report" reportGeneratorSpec
              describe "Tenant" $ do
                describe "Config" tenantConfigValidationSpec
                tenantValidationSpec
          before (resetDB appContext) $ describe "INTEGRATION TESTING" $ do
            describe "API" $ do
              apiKeyAPI baseContext appContext
              appKeyAPI baseContext appContext
              configAPI baseContext appContext
              documentAPI baseContext appContext
              documentTemplateAPI baseContext appContext
              documentTemplateDraftAPI baseContext appContext
              documentTemplateDraftFolderAPI baseContext appContext
              documentTemplateDraftAssetAPI baseContext appContext
              documentTemplateDraftFileAPI baseContext appContext
              domainAPI baseContext appContext
              externalLinkAPI baseContext appContext
              feedbackAPI baseContext appContext
              infoAPI baseContext appContext
              knowledgeModelAPI baseContext appContext
              knowledgeModelEditorAPI baseContext appContext
              knowledgeModelPackageAPI baseContext appContext
              knowledgeModelSecretAPI baseContext appContext
              localeAPI baseContext appContext
              prefabAPI baseContext appContext
              projectAPI baseContext appContext
              projectActionAPI baseContext appContext
              projectCommentThreadAPI baseContext appContext
              projectImporterAPI baseContext appContext
              submissionAPI baseContext appContext
              swaggerAPI baseContext appContext
              tenantAPI baseContext appContext
              tenantConfigAPI baseContext appContext
              tenantLimitAPI baseContext appContext
              typeHintAPI baseContext appContext
              tokenAPI baseContext appContext
              usageAPI baseContext appContext
              userAPI baseContext appContext
              userNewsAPI baseContext appContext
              userTourAPI baseContext appContext
              userGroupAPI baseContext appContext
            describe "SERVICE" $ do
              documentIntegrationSpec appContext
              describe "KnowledgeModel" $ do
                describe "Editor" $ do
                  describe "Migration" $ do
                    migratorSpec appContext
                    KM_SanitizerSpec.sanitizerSpec appContext
                  knowledgeModelEditorServiceSpec appContext
                describe "Package" $ packageValidationSpec appContext
              describe "Project" $ do
                describe "Migration" $
                  PRJ_SanitizerSpec.sanitizerIntegrationSpec appContext
                projectAclSpec appContext
                projectCollaborationAclSpec appContext
                projectServiceSpec appContext
              userServiceIntegrationSpec appContext
            describe "UTIL" $ do
              jinjaSpec
            describe "WEBSOCKET" $ do
              knowledgeModelEditorWebsocketAPI appContext
              projectWebsocketAPI appContext
    )
