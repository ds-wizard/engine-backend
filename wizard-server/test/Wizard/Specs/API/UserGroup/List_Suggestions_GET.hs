module Wizard.Specs.API.UserGroup.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Service.User.Group.UserGroupMapper
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/user-groups/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/user-groups/suggestions" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/user-groups/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/user-groups/suggestions?sort=uuid,asc"
    (Page "userGroups" (PageMetadata 20 2 1 0) [toSuggestion bioGroup, toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (pagination)"
    appContext
    "/wizard-api/user-groups/suggestions?sort=uuid,asc&page=1&size=1"
    (Page "userGroups" (PageMetadata 1 2 2 1) [toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (query)"
    appContext
    "/wizard-api/user-groups/suggestions?sort=uuid,asc&q=plan"
    (Page "userGroups" (PageMetadata 20 1 1 0) [toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (sort asc)"
    appContext
    "/wizard-api/user-groups/suggestions?sort=name,asc"
    (Page "userGroups" (PageMetadata 20 2 1 0) [toSuggestion bioGroup, toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (sort desc)"
    appContext
    "/wizard-api/user-groups/suggestions?sort=name,desc"
    (Page "userGroups" (PageMetadata 20 2 1 0) [toSuggestion plantGroup, toSuggestion bioGroup])

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
