module Main where

import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Web.Scotty as S

import Application
import Context
import DSPConfig

import Specs.API.EventAPISpec
import Specs.API.InfoAPISpec
import Specs.API.KnowledgeModelAPISpec
import Specs.API.KnowledgeModelContainerAPISpec
import Specs.API.OrganizationAPISpec
import Specs.API.PackageAPISpec
import Specs.API.TokenAPISpec
import Specs.API.UserAPISpec
import Specs.API.VersionAPISpec
import Specs.Service.KMMigration.ApplicatorINTSpec
import Specs.Service.KMMigration.ApplicatorSpec
import Specs.Service.Organization.OrganizationServiceSpec
import Specs.Service.Package.PackageServiceSpec
import Specs.Service.KnowledgeModelContainerService.KnowledgeModelContainerServiceSpec
import TestMigration

testApplicationConfigFile = "config/app-config-test.cfg"

testBuildInfoFile = "config/build-info-test.cfg"

prepareWebApp runCallback = do
  eitherDspConfig <- loadDSPConfig testApplicationConfigFile testBuildInfoFile
  case eitherDspConfig of
    Left (errorDate, reason) -> do
      putStrLn "CONFIG: load failed"
      putStrLn
        "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
      print errorDate
    Right dspConfig -> do
      putStrLn "CONFIG: loaded"
      createDBConn dspConfig $ \dbPool -> do
        putStrLn "DATABASE: connected"
        let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
        runCallback context dspConfig

main :: IO ()
main =
  prepareWebApp
    (\context dspConfig ->
       hspec $ do
         describe "UNIT TESTING" $ do
           applicatorSpec
           applicatorINTSpec
           organizationServiceSpec
           knowledgeModelContainerServiceSpec
           packageServiceSpec
         before (resetDB context dspConfig) $
           describe "INTEGRATION TESTING" $ do
             infoAPI context dspConfig
             tokenAPI context dspConfig
             organizationAPI context dspConfig
             userAPI context dspConfig
             kmcAPI context dspConfig
             knowledgeModelAPI context dspConfig
             eventAPI context dspConfig
             versionAPI context dspConfig
             packageAPI context dspConfig)
