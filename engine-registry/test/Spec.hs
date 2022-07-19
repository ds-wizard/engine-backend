module Main where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.UUID as U
import Test.Hspec

import LensesConfig
import Registry.Constant.Resource
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Service.Config.ServerConfigService
import Shared.Database.Connection
import Shared.Integration.Http.Common.HttpClientFactory
import Shared.S3.Common
import Shared.Service.Config.BuildInfoConfigService

import Registry.Specs.API.ActionKey.APISpec
import Registry.Specs.API.Info.APISpec
import Registry.Specs.API.Organization.APISpec
import Registry.Specs.API.Package.APISpec
import Registry.Specs.API.Template.APISpec
import Registry.Specs.Service.Organization.OrganizationValidationSpec
import Registry.Specs.Service.Package.PackageValidationSpec
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
  hLoadConfig serverConfigFileTest getServerConfig $ \serverConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      putStrLn $ "ENVIRONMENT: set to " `mappend` show (serverConfig ^. general . environment)
      dbPool <- createDatabaseConnectionPool (serverConfig ^. database)
      putStrLn "DATABASE: connected"
      httpClientManager <- createHttpClientManager (serverConfig ^. logging)
      putStrLn "HTTP_CLIENT: created"
      s3Client <- createS3Client (serverConfig ^. s3) httpClientManager
      putStrLn "S3_CLIENT: created"
      let baseContext =
            BaseContext
              { _baseContextServerConfig = serverConfig
              , _baseContextLocalization = M.empty
              , _baseContextBuildInfoConfig = buildInfoConfig
              , _baseContextDbPool = dbPool
              , _baseContextS3Client = s3Client
              }
      withResource dbPool $ \dbConnection -> do
        let appContext =
              AppContext
                { _appContextServerConfig = serverConfig
                , _appContextLocalization = M.empty
                , _appContextBuildInfoConfig = buildInfoConfig
                , _appContextDbPool = dbPool
                , _appContextDbConnection = Just dbConnection
                , _appContextS3Client = s3Client
                , _appContextTraceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
                , _appContextCurrentOrganization = Just orgGlobal
                }
        buildSchema appContext
        runCallback baseContext appContext

main :: IO ()
main =
  prepareWebApp
    (\baseContext appContext ->
       hspec $ do
         describe "UNIT TESTING" $ describe "SERVICE" $ do
           describe "Organization" organizationValidationSpec
           describe "Package" packageValidationSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ describe "API" $ do
           actionKeyAPI baseContext appContext
           infoAPI baseContext appContext
           organizationAPI baseContext appContext
           packageAPI baseContext appContext
           templateAPI baseContext appContext)
