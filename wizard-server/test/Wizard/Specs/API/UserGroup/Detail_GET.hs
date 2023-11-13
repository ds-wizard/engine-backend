module Wizard.Specs.API.UserGroup.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Service.User.UserMapper
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailJM ()
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroupMembership
import WizardLib.Public.Service.User.Group.UserGroupMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/user-groups/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/user-groups/{uuid}" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/user-groups/6ab8ecc3-c0f5-4864-a055-b0096ca55569"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = toDetailDTO bioGroup [toWithMembership userNikola OwnerUserGroupMembershipType, toWithMembership userAlbert OwnerUserGroupMembershipType]
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/user-groups/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "user_group"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
