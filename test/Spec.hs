module Main where

import Data.Aeson (Value(..), (.=), object)
import Network.Wai (Application)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Web.Scotty as S

import Common.ConfigLoader
import Common.Context
import Database.Connection
import Model.Config.DSWConfig

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
import Specs.Model.KnowledgeModel.KnowledgeModelSpec
import Specs.Service.Branch.BranchServiceSpec
import Specs.Service.Migrator.ApplicatorSpec
import Specs.Service.Migrator.MigratorSpec
import Specs.Service.Migrator.SanitizatorSpec
import Specs.Service.Organization.OrganizationServiceSpec
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
      createDBConn dswConfig $ \dbPool -> do
        putStrLn "DATABASE: connected"
        let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
        runCallback context dswConfig

main :: IO ()
main =
  prepareWebApp
    (\context dswConfig ->
       hspec $ do
         describe "UNIT TESTING" $ do
           commonUtilsSpec
           applicatorSpec
           knowledgeModelSpec
           sanitizatorSpec
           migratorSpec
           organizationServiceSpec
           branchServiceSpec
           packageServiceSpec
         before (resetDB context dswConfig) $
           describe "INTEGRATION TESTING" $ do
             describe "Service tests" $ branchServiceIntegrationSpec context dswConfig
             describe "API Tests" $ do
               infoAPI context dswConfig
               tokenAPI context dswConfig
               organizationAPI context dswConfig
               userAPI context dswConfig
               branchAPI context dswConfig
               knowledgeModelAPI context dswConfig
               eventAPI context dswConfig
               versionAPI context dswConfig
               packageAPI context dswConfig
               migratorAPI context dswConfig)
