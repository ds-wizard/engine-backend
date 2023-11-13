module Wizard.Specs.API.Token.List_DELETE (
  list_DELETE,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Model.User.UserToken

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/tokens
-- ------------------------------------------------------------------------
list_DELETE :: AppContext -> SpecWith ((), Application)
list_DELETE appContext =
  describe "DELETE /wizard-api/tokens" $ do
    test_204 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/wizard-api/tokens"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migration
      eUser <- runInContextIO (insertUserToken alternativeAlbertToken) appContext
      assertUserTokenInDB appContext userAlbert 2
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertUserTokenInDB appContext userAlbert 1
      assertExistenceOfUserTokenInDB appContext userAlbert albertToken.value

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
