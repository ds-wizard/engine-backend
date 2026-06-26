module Wizard.Specs.API.Role.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Wizard.Database.Migration.Development.User.Data.Roles
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import qualified WizardLib.Public.Service.User.RoleMapper as Mapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/roles
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/roles" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/roles"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (sort by uuid asc)"
    appContext
    "/wizard-api/roles?sort=uuid,asc"
    ( Page
        "roles"
        (PageMetadata 20 3 1 0)
        [Mapper.toDTO adminRole 2, Mapper.toDTO dataStewardRole 1, Mapper.toDTO researcherRole 1]
    )
  create_test_200
    "HTTP 200 OK (pagination)"
    appContext
    "/wizard-api/roles?sort=uuid,asc&page=1&size=1"
    (Page "roles" (PageMetadata 1 3 3 1) [Mapper.toDTO dataStewardRole 1])
  create_test_200
    "HTTP 200 OK (query)"
    appContext
    "/wizard-api/roles?sort=uuid,asc&q=Researcher"
    (Page "roles" (PageMetadata 20 1 1 0) [Mapper.toDTO researcherRole 1])

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "SettingsManageRolePermission"
