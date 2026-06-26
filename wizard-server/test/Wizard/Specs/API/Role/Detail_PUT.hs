module Wizard.Specs.API.Role.Detail_PUT (
  detail_PUT,
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
import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Api.Resource.User.RoleChangeJM ()
import WizardLib.Public.Api.Resource.User.RoleListJM ()
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RoleList
import WizardLib.Public.Model.User.RolePermission

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Role.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/roles/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/roles/{uuid}" $ do
    test_200 appContext
    test_400_invalid appContext
    test_400_admin appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/roles/a0000000-0000-0000-0000-000000000002"

reqUrlAdmin = "/wizard-api/roles/a0000000-0000-0000-0000-000000000001"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: RoleChangeDTO
reqDto =
  RoleChangeDTO
    { name = "Data Steward (edited)"
    , permissions = [_KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto =
          RoleList
            { uuid = dataStewardRole.uuid
            , name = reqDto.name
            , permissions = reqDto.permissions
            , usersCount = 1
            , isAdmin = False
            }
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, RoleList)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareRoleDtos resDto expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_admin appContext =
  it "HTTP 400 BAD REQUEST when the admin role is changed" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_CHANGED
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlAdmin reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "SettingsManageRolePermission"

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
