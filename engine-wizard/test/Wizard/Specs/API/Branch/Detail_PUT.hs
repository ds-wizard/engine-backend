module Wizard.Specs.API.Branch.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.BranchList
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.Branch.BranchService

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Branch.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /branches/{branchId}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /branches/{branchId}" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_400_not_valid_kmId appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = amsterdamBranchChange

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = amsterdamBranchDetail
      -- AND: Run migrations
      runInContextIO
        ( createBranchWithParams
            amsterdamBranchList.uuid
            amsterdamBranchList.createdAt
            (fromJust appContext.currentUser)
            amsterdamBranchCreate
        )
        appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, BranchDetailDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareBranchDtos
        resBody
        reqDto
        resBody.previousPackageId
        resBody.previousPackageId
        (Just userAlbert.uuid)
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfBranchInDB
        appContext
        reqDto
        resBody.previousPackageId
        resBody.previousPackageId
        (Just userAlbert.uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl "kmId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_valid_kmId appContext =
  it "HTTP 400 BAD REQUEST when kmId is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamBranchChange {kmId = "amsterdam.km-"} :: BranchChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "kmId" [_ERROR_VALIDATION__INVALID_KM_ID_FORMAT])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO
        ( createBranchWithParams
            amsterdamBranchList.uuid
            amsterdamBranchList.createdAt
            (fromJust appContext.currentUser)
            amsterdamBranchCreate
        )
        appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfBranchInDB
        appContext
        amsterdamBranchList
        amsterdamBranchList.previousPackageId
        amsterdamBranchList.previousPackageId
        (Just userAlbert.uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "branch"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
