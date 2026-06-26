module Wizard.Specs.API.Role.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Error.Error
import Wizard.Database.Migration.Development.User.Data.Roles
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.DAO.User.RoleDAO (insertRole)
import WizardLib.Public.Model.User.Role

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Role.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/roles/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith ((), Application)
detail_DELETE appContext =
  describe "DELETE /wizard-api/roles/{uuid}" $ do
    test_204 appContext
    test_400_admin appContext
    test_400_default appContext
    test_400_in_use appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/wizard-api/roles/a0000000-0000-0000-0000-0000000000ff"

reqHeaders = [reqAuthHeader, reqCtHeader]

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
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO (insertRole deletableRole) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfRoleInDB appContext deletableRole.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_admin appContext = createDeleteValidationTest appContext "the admin role is deleted" "a0000000-0000-0000-0000-000000000001" _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_DELETED

test_400_default appContext = createDeleteValidationTest appContext "the default role is deleted" "a0000000-0000-0000-0000-000000000003" _ERROR_VALIDATION__USER_ROLE_IS_DEFAULT

test_400_in_use appContext = createDeleteValidationTest appContext "the role is assigned to users" "a0000000-0000-0000-0000-000000000002" _ERROR_VALIDATION__USER_ROLE_IN_USE

createDeleteValidationTest appContext title roleUuid expError =
  it ("HTTP 400 BAD REQUEST when " ++ title) $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError expError
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod ("/wizard-api/roles/" `mappend` roleUuid) reqHeaders reqBody
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "SettingsManageRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/wizard-api/roles/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "role"
    [("tenant_uuid", "00000000-0000-0000-0000-000000000000"), ("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
