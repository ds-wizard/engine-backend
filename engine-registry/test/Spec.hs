module Main where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec

import LensesConfig
import Registry.Constant.Resource
import Registry.Database.Connection
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Service.Config.ApplicationConfigService
import Registry.Service.Config.BuildInfoConfigService

import Registry.Specs.API.ActionKey.APISpec
import Registry.Specs.API.Info.APISpec
import Registry.Specs.API.Organization.APISpec
import Registry.Specs.API.Package.APISpec
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
  hLoadConfig applicationConfigFileTest getApplicationConfig $ \appConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      putStrLn $ "ENVIRONMENT: set to " `mappend` show (appConfig ^. general . environment)
      dbPool <- createDatabaseConnectionPool appConfig
      putStrLn "DATABASE: connected"
      let appContext =
            AppContext
              { _appContextApplicationConfig = appConfig
              , _appContextLocalization = M.empty
              , _appContextBuildInfoConfig = buildInfoConfig
              , _appContextPool = dbPool
              , _appContextTraceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
              , _appContextCurrentOrganization = Just orgGlobal
              }
      runCallback appContext

main :: IO ()
main =
  prepareWebApp
    (\appContext ->
       hspec $ do
         describe "UNIT TESTING" $ describe "SERVICE" $ do
           describe "Organization" organizationValidationSpec
           describe "Package" packageValidationSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ describe "API" $ do
           actionKeyAPI appContext
           infoAPI appContext
           organizationAPI appContext
           packageAPI appContext)
