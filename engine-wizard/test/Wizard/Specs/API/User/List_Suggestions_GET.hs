module Wizard.Specs.API.User.List_Suggestions_GET
  ( list_suggestions_GET
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Service.User.UserMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /users/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /users/suggestions" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/users/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Admin)"
    appContext
    "/users/suggestions?sort=uuid,asc"
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    appContext
    "/users/suggestions?sort=uuid,asc&page=1&size=1"
    (Page "users" (PageMetadata 1 3 3 1) [toSuggestionDTO . toSuggestion $ userIsaac])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    appContext
    "/users/suggestions?sort=uuid,asc&q=te"
    (Page "users" (PageMetadata 20 2 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userAlbert]))
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    appContext
    "/users/suggestions?sort=first_name,asc"
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userAlbert, userIsaac, userNikola]))
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    appContext
    "/users/suggestions?sort=first_name,desc"
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userIsaac, userAlbert]))

create_test_200 title appContext reqUrl expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody
