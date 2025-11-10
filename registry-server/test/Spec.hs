module Main where

import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.UUID as U
import Test.Hspec

import Registry.Constant.Resource
import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigIM ()
import Registry.Model.Config.ServerConfigJM ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Service.Config.Server.ServerConfigValidation
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Database.Connection
import Shared.Common.Integration.Http.Common.HttpClientFactory
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.S3.Common
import Shared.Common.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Common.Service.Config.Server.ServerConfigService

import Registry.Specs.API.ActionKey.APISpec
import Registry.Specs.API.Config.APISpec
import Registry.Specs.API.DocumentTemplate.APISpec
import Registry.Specs.API.Info.APISpec
import Registry.Specs.API.KnowledgeModelPackage.APISpec
import Registry.Specs.API.Locale.APISpec
import Registry.Specs.API.Organization.APISpec
import Registry.Specs.Service.KnowledgeModel.Package.PackageValidationSpec
import Registry.TestMigration

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
      putStrLn $ "ENVIRONMENT: set to " `mappend` serverConfig.general.environment
      dbPool <- createDatabaseConnectionPool serverConfig.database
      putStrLn "DATABASE: connected"
      httpClientManager <- createHttpClientManager serverConfig.logging
      putStrLn "HTTP_CLIENT: created"
      s3Client <- createS3Client serverConfig.s3 httpClientManager
      putStrLn "S3_CLIENT: created"
      let baseContext =
            BaseContext
              { serverConfig = serverConfig
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              , httpClientManager = httpClientManager
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
                , traceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
                , currentOrganization = Just orgGlobal
                }
        buildSchema appContext
        runCallback baseContext appContext

main :: IO ()
main =
  prepareWebApp
    ( \baseContext appContext ->
        hspec $ do
          describe "UNIT TESTING" $ describe "SERVICE" $ do
            describe "KnowledgeModel" $
              describe
                "Package"
                packageValidationSpec
          before (resetDB appContext) $ describe "INTEGRATION TESTING" $ describe "API" $ do
            actionKeyAPI baseContext appContext
            configAPI baseContext appContext
            infoAPI baseContext appContext
            knowledgeModelPackageAPI baseContext appContext
            localeAPI baseContext appContext
            organizationAPI baseContext appContext
            templateAPI baseContext appContext
    )
