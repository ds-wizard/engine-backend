module Wizard.Specs.API.Config.List_Locale_GET (
  list_locale_GET,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Util.String (f')
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/configs/locales/{localeId}
-- ------------------------------------------------------------------------
list_locale_GET :: AppContext -> SpecWith ((), Application)
list_locale_GET appContext = describe "GET /wizard-api/configs/locales/{localeId}" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT localeId = BS.pack $ f' "/wizard-api/configs/locales/%s" [localeId]

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (nl-NL)" appContext "nl-NL"
  create_test_200 "HTTP 200 OK (nl)" appContext "nl"

create_test_200 title appContext localeId = do
  it title $
    -- GIVEN: Prepare expectation
    do
      let reqUrl = reqUrlT localeId
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict localeNlContent
      -- AND: Run migrations
      runInContextIO LOC.runMigration appContext
      runInContextIO LOC.runS3Migration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
