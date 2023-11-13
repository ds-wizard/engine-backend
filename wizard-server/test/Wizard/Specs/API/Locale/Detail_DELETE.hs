module Wizard.Specs.API.Locale.Detail_DELETE (
  detail_DELETE,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Database.DAO.Locale.LocaleDAO
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/locales/{lclId}
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith ((), Application)
detail_DELETE appContext =
  describe "DELETE /wizard-api/locales/{lclId}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/wizard-api/locales/global:dutch:1.0.0"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
    -- AND: Run migrations
    runInContextIO LOC_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findLocales appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "LOC_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/locales/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "locale"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
