module Main where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec

import Constant.Resource
import Database.Connection
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Model.Context.AppContext
import Service.Config.ApplicationConfigService
import Service.Config.BuildInfoConfigService

import Specs.API.ActionKey.APISpec
import Specs.API.Info.APISpec
import Specs.API.Organization.APISpec
import Specs.API.Package.APISpec
import Specs.Service.Organization.OrganizationValidationSpec
import Specs.Service.Package.PackageValidationSpec
import Specs.Util.ListSpec
import Specs.Util.MathSpec
import TestMigration

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

prepareWebApp runCallback = do
  hLoadConfig applicationConfigFileTest getApplicationConfig $ \appConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      putStrLn $ "ENVIRONMENT: set to " `mappend` (show $ appConfig ^. general . environment)
      dbPool <- createDatabaseConnectionPool appConfig
      putStrLn "DATABASE: connected"
      let appContext =
            AppContext
            { _appContextAppConfig = appConfig
            , _appContextBuildInfoConfig = buildInfoConfig
            , _appContextPool = dbPool
            , _appContextTraceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
            , _appContextCurrentOrganization = Just orgDsw
            }
      runCallback appContext

main :: IO ()
main =
  prepareWebApp
    (\appContext ->
       hspec $ do
         describe "UNIT TESTING" $ do
           describe "SERVICE" $ do
             describe "Organization" $ organizationValidationSpec
             describe "Package" $ packageValidationSpec
           describe "UTIL" $ do
             listSpec
             mathSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ do
           describe "API" $ do
             actionKeyAPI appContext
             infoAPI appContext
             organizationAPI appContext
             packageAPI appContext)
