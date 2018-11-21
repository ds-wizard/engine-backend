module Main where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec

import Database.Connection
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Messaging.Connection
import Model.Context.AppContext
import Service.Config.ConfigLoader
import Service.User.UserMapper

import Specs.API.BookReference.APISpec
import Specs.API.BranchAPISpec
import Specs.API.EventAPISpec
import Specs.API.Feedback.APISpec
import Specs.API.InfoAPISpec
import Specs.API.KnowledgeModelAPISpec
import Specs.API.Level.APISpec
import Specs.API.Metric.APISpec
import Specs.API.MigratorAPISpec
import Specs.API.OrganizationAPISpec
import Specs.API.Package.APISpec
import Specs.API.Questionnaire.APISpec
import Specs.API.TokenAPISpec
import Specs.API.UserAPISpec
import Specs.API.VersionAPISpec
import Specs.Model.FilledKnowledgeModel.FilledKnowledgeModelAccessorsSpec
import Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Specs.Service.Branch.BranchServiceSpec
import Specs.Service.Branch.BranchValidationSpec
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
        msgChannel <- createMessagingChannel dswConfig
        let appContext =
              AppContext
              { _appContextConfig = dswConfig
              , _appContextPool = dbPool
              , _appContextMsgChannel = msgChannel
              , _appContextTraceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
              , _appContextCurrentUser = Just . toDTO $ userAlbert
              }
        runCallback appContext

main :: IO ()
main =
  prepareWebApp
    (\baseContext ->
       hspec $ do
         describe "UNIT TESTING" $ do
           describe "MODEL" $ do
             filledKnowledgeModelAccessorsSpec
             knowledgeModelAccessorsSpec
           describe "SERVICE" $ do
             describe "Branch" $ do branchValidationSpec
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
         before (resetDB baseContext) $ describe "INTEGRATION TESTING" $ do
           describe "API" $ do
             bookReferenceAPI baseContext
             branchAPI baseContext
             eventAPI baseContext
             feedbackAPI baseContext
             infoAPI baseContext
             knowledgeModelAPI baseContext
             levelAPI baseContext
             metricAPI baseContext
             migratorAPI baseContext
             organizationAPI baseContext
             packageAPI baseContext
             questionnaireAPI baseContext
             tokenAPI baseContext
             userAPI baseContext
             versionAPI baseContext
           describe "SERVICE" $ branchServiceIntegrationSpec baseContext)
