module Wizard.Specs.API.Role.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Error.Error
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Api.Resource.User.RoleChangeJM ()
import WizardLib.Public.Api.Resource.User.RoleListJM ()
import WizardLib.Public.Database.DAO.User.RoleDAO (findRoles)
import WizardLib.Public.Model.User.RoleList
import WizardLib.Public.Model.User.RolePermission

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Role.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/roles
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/roles" $ do
    test_201 appContext
    test_400 appContext
    test_400_invalid_permission appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/roles"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: RoleChangeDTO
reqDto =
  RoleChangeDTO
    { name = "Reviewer"
    , permissions = [_PROJECTS_VIEW_ROLE_PERMISSION]
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto =
          RoleList
            { uuid = U.nil
            , name = reqDto.name
            , permissions = reqDto.permissions
            , usersCount = 0
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
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findRoles appContext 4

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_permission appContext = do
  createInvalidPermissionTest appContext "an internal permission is used" _DEV_USE_ROLE_PERMISSION
  createInvalidPermissionTest appContext "an unknown permission is used" "NonsenseRolePermission"

createInvalidPermissionTest appContext title permission =
  it ("HTTP 400 BAD REQUEST when " ++ title) $ do
    -- GIVEN: Prepare request with an invalid permission
    let invalidReqBody = encode (reqDto {permissions = [permission]} :: RoleChangeDTO)
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError $ _ERROR_VALIDATION__USER_ROLE_INVALID_PERMISSION permission
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders invalidReqBody
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
