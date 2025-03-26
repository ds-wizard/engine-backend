module Wizard.Specs.API.User.List_Current_Locale_PUT (
  list_current_locale_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.Migration.Development.User.Data.Users

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current/locale
-- ------------------------------------------------------------------------
list_current_locale_PUT :: AppContext -> SpecWith ((), Application)
list_current_locale_PUT appContext =
  describe "PUT /wizard-api/users/current/locale" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/current/locale"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userLocale

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = userLocale
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserInDB appContext userAlbertEditedLocale

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
