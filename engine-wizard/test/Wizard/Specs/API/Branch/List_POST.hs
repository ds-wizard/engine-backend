module Wizard.Specs.API.Branch.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.Branch.BranchList
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Branch.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /branches
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /branches" $ do
    test_201 appContext
    test_400_invalid_json appContext
    test_400_not_valid_kmId appContext
    test_400_not_existing_previousPackageId appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/branches"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = amsterdamBranchCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = amsterdamBranchDetail
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, BranchList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareBranch
        resBody
        reqDto
        reqDto.previousPackageId
        reqDto.previousPackageId
        (Just userAlbert.uuid)
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findBranches appContext 1
      assertExistenceOfBranchInDB
        appContext
        reqDto
        reqDto.previousPackageId
        reqDto.previousPackageId
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
      let reqDto = amsterdamBranchCreate {kmId = "amsterdam.km-"} :: BranchCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "kmId" [_ERROR_VALIDATION__INVALID_KM_ID_FORMAT])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findBranches appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_existing_previousPackageId appContext =
  it "HTTP 400 BAD REQUEST when previousPackageId does not exist" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamBranchCreate {previousPackageId = Just "org.nl:core-nl:9.9.9"} :: BranchCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "previousPackageId" [_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findBranches appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_PERM"
