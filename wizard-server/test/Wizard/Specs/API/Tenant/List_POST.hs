module Wizard.Specs.API.Tenant.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Model.Error.Error
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantJM ()
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/tenants
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/tenants" $ do
    test_201 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/tenants"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT dto = dto

reqBodyT dto = encode (reqDtoT dto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (anonymous)" appContext tenantCreateDto [] 2 False
  create_test_201 "HTTP 201 CREATED (admin)" appContext tenantCreateDto [reqAuthHeader] 1 True

create_test_201 title appContext reqDto authHeaders persistentCommandCount userActive =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqBody = reqBodyT reqDto
      -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TenantDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Find result in DB and compare with expectation state
      (Right tenant) <- runInContextIO (findTenantByClientUrl resDto.clientUrl) appContext
      let updatedAppContext = appContext {currentTenantUuid = tenant.uuid}
      (Right [user]) <- runInContextIO findUsers updatedAppContext
      liftIO $ user.active `shouldBe` userActive
      assertCountInDB (findActionKeys :: AppContextM [ActionKey U.UUID ActionKeyType]) updatedAppContext 1
      assertCountInDB (findPersistentCommands :: AppContextM [PersistentCommand U.UUID]) updatedAppContext persistentCommandCount

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  create_test_400_tenant_id_uniqueness "HTTP 400 BAD REQUEST if tenantId is already used (anonymous)" appContext []
  create_test_400_tenant_id_uniqueness "HTTP 400 BAD REQUEST if tenantId is already used (admin)" appContext [reqAuthHeader]

create_test_400_tenant_id_uniqueness title appContext authHeaders =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqDto = tenantCreateDto {tenantId = "default"} :: TenantCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "tenantId" [_ERROR_VALIDATION__TENANT_ID_UNIQUENESS])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
