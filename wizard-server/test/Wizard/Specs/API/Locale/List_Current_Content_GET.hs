module Wizard.Specs.API.Locale.List_Current_Content_GET (
  list_current_content_GET,
) where

import Control.Monad (void, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/locales/current/content
-- ------------------------------------------------------------------------
list_current_content_GET :: AppContext -> SpecWith ((), Application)
list_current_content_GET appContext = describe "GET /wizard-api/locales/current/content" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/locales/current/content"

reqHeadersT authHeaders = authHeaders

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (anonymous)" appContext [] "{}" False
  create_test_200 "HTTP 200 OK (authorized - use default)" appContext [reqAuthHeader] "{}" False
  create_test_200 "HTTP 200 OK (authorized - selected NL)" appContext [reqAuthHeader] (BSL.fromStrict localeNlContent) True

create_test_200 title appContext authHeaders expBody changeUserLocale = do
  it title $
    -- GIVEN: Prepare variables
    do
      let reqHeaders = reqHeadersT authHeaders
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO LOC.runMigration appContext
      runInContextIO LOC.runS3Migration appContext
      when changeUserLocale (void $ runInContextIO (updateUserByUuid userAlbertEditedLocale) appContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
