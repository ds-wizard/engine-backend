module Wizard.Specs.API.Config.List_Bootstrap_GET (
  list_bootstrap_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.Migration.Development.Locale.Data.Locales
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.ClientConfigMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /configs/bootstrap
-- ------------------------------------------------------------------------
list_bootstrap_GET :: AppContext -> SpecWith ((), Application)
list_bootstrap_GET appContext = describe "GET /configs/bootstrap" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/configs/bootstrap"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toClientConfigDTO appContext.serverConfig defaultAppConfig defaultApp [localeDefaultEn, localeNl]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
