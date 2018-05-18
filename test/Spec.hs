module Main where

import Control.Lens ((^.))
import Test.Hspec

import Common.ConfigLoader
import Database.Connection
import LensesConfig
import Model.Context.AppContext

import Specs.API.BranchAPISpec
import Specs.API.EventAPISpec
import Specs.API.InfoAPISpec
import Specs.API.KnowledgeModelAPISpec
import Specs.API.MigratorAPISpec
import Specs.API.OrganizationAPISpec
import Specs.API.PackageAPISpec
import Specs.API.TokenAPISpec
import Specs.API.UserAPISpec
import Specs.API.VersionAPISpec
import Specs.Common.UtilsSpec
import Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Specs.Service.Branch.BranchServiceSpec
import Specs.Service.Migrator.ApplicatorSpec
import Specs.Service.Migrator.MigratorSpec
import Specs.Service.Migrator.SanitizatorSpec
import Specs.Service.Organization.OrganizationValidationSpec
import Specs.Service.Package.PackageServiceSpec
import TestMigration

testApplicationConfigFile = "config/app-config-test.cfg"

testBuildInfoFile = "config/build-info-test.cfg"

prepareWebApp runCallback = do
  eitherDspConfig <- loadDSWConfig testApplicationConfigFile testBuildInfoFile
  case eitherDspConfig of
    Left (errorDate, reason) -> do
      putStrLn "CONFIG: load failed"
      putStrLn "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
      print errorDate
    Right dswConfig -> do
      putStrLn "CONFIG: loaded"
      putStrLn $ "ENVIRONMENT: set to " `mappend` (show $ dswConfig ^. environment . env)
      createDBConn dswConfig $ \dbPool -> do
        putStrLn "DATABASE: connected"
        let appContext = AppContext {_appContextConfig = dswConfig, _appContextPool = dbPool}
        runCallback appContext

main :: IO ()
main =
  prepareWebApp
    (\appContext ->
       hspec $ do
         describe "UNIT TESTING" $ do
           commonUtilsSpec
           applicatorSpec
           knowledgeModelAccessorsSpec
           sanitizatorSpec
           migratorSpec
           organizationValidationSpec
           branchServiceSpec
           packageServiceSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ do
           describe "Service tests" $ branchServiceIntegrationSpec appContext
           describe "API Tests" $ do
             infoAPI appContext
             tokenAPI appContext
             organizationAPI appContext
             userAPI appContext
             branchAPI appContext
             knowledgeModelAPI appContext
             eventAPI appContext
             versionAPI appContext
             packageAPI appContext
             migratorAPI appContext)
