module Main where

import Control.Lens ((^.))
import Test.Hspec

import Database.Connection
import LensesConfig
import Model.Context.AppContext
import Service.Config.ConfigLoader

import Specs.API.BookReference.APISpec
import Specs.API.BranchAPISpec
import Specs.API.EventAPISpec
import Specs.API.Feedback.APISpec
import Specs.API.InfoAPISpec
import Specs.API.KnowledgeModelAPISpec
import Specs.API.Metric.APISpec
import Specs.API.MigratorAPISpec
import Specs.API.OrganizationAPISpec
import Specs.API.PackageAPISpec
import Specs.API.Questionnaire.APISpec
import Specs.API.TokenAPISpec
import Specs.API.UserAPISpec
import Specs.API.VersionAPISpec
import Specs.Model.FilledKnowledgeModel.FilledKnowledgeModelAccessorsSpec
import Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Specs.Service.Branch.BranchServiceSpec
import Specs.Service.DataManagementPlan.DataManagementPlanServiceSpec
import Specs.Service.Migrator.ApplicatorSpec
import Specs.Service.Migrator.MigratorSpec
import Specs.Service.Migrator.SanitizatorSpec
import Specs.Service.Organization.OrganizationValidationSpec
import Specs.Service.Package.PackageValidationSpec
import Specs.Service.Token.TokenServiceSpec
import Specs.Util.ListSpec
import Specs.Util.MathSpec
import Specs.Util.TokenSpec
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
           describe "MODEL" $ do
             filledKnowledgeModelAccessorsSpec
             knowledgeModelAccessorsSpec
           describe "SERVICE" $ do
             describe "Branch" $ branchServiceSpec
             describe "DataManagementPlan" $ dataManagementPlanSpec
             describe "Migrator" $ do
               applicatorSpec
               migratorSpec
               sanitizatorSpec
             describe "Organization" $ organizationValidationSpec
             describe "Package" $ packageValidationSpec
             describe "Token" $ tokenServiceSpec
           describe "UTIL" $ do
             listSpec
             mathSpec
             tokenSpec
         before (resetDB appContext) $ describe "INTEGRATION TESTING" $ do
           describe "API" $ do
             bookReferenceAPI appContext
             branchAPI appContext
             eventAPI appContext
             feedbackAPI appContext
             infoAPI appContext
             knowledgeModelAPI appContext
             metricAPI appContext
             migratorAPI appContext
             organizationAPI appContext
             packageAPI appContext
             questionnaireAPI appContext
             tokenAPI appContext
             userAPI appContext
             versionAPI appContext
           describe "SERVICE" $ branchServiceIntegrationSpec appContext)
