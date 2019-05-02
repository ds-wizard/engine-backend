module Specs.API.Branch.Detail_PUT
  ( detail_put
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Data.Maybe (fromJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Branch.BranchDTO
import Api.Resource.Error.ErrorDTO ()
import Database.Migration.Development.Branch.Data.Branches
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.ErrorHelpers
import Service.Branch.BranchService

import Specs.API.Branch.Common
import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /branches/{branchId}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith Application
detail_put appContext =
  describe "PUT /branches/{branchId}" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_400_not_valid_kmId appContext
    test_400_already_taken_kmId appContext
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
test_200 appContext = do
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = amsterdamBranchDetail
     -- AND: Run migrations
    runInContextIO
      (createBranchWithParams
         (amsterdamBranch ^. uuid)
         (amsterdamBranch ^. createdAt)
         (fromJust $ appContext ^. currentUser)
         amsterdamBranchCreate)
      appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, BranchDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareBranchDtos
      resBody
      reqDto
      (resBody ^. parentPackageId)
      (resBody ^. parentPackageId)
      (Just $ userAlbert ^. uuid)
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfBranchInDB
      appContext
      reqDto
      (resBody ^. parentPackageId)
      (resBody ^. parentPackageId)
      (Just $ userAlbert ^. uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common KM" } |] "kmId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_valid_kmId appContext = do
  it "HTTP 400 BAD REQUEST when kmId is not in valid format" $
     -- GIVEN: Prepare request
   do
    let reqDto = amsterdamBranchChange & kmId .~ "amsterdam.km-"
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithFieldError ("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO
      (createBranchWithParams
         (amsterdamBranch ^. uuid)
         (amsterdamBranch ^. createdAt)
         (fromJust $ appContext ^. currentUser)
         amsterdamBranchCreate)
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
      amsterdamBranch
      (amsterdamBranch ^. parentPackageId)
      (amsterdamBranch ^. parentPackageId)
      (Just $ userAlbert ^. uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_already_taken_kmId appContext = do
  it "HTTP 400 BAD REQUEST when kmId is already taken" $
     -- GIVEN: Prepare request
   do
    let reqDto = amsterdamBranchChange & kmId .~ (leidenBranch ^. kmId)
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithFieldError ("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS $ reqDto ^. kmId)
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO
      (createBranchWithParams
         (amsterdamBranch ^. uuid)
         (amsterdamBranch ^. createdAt)
         (fromJust $ appContext ^. currentUser)
         amsterdamBranchCreate)
      appContext
    runInContextIO
      (createBranchWithParams
         (leidenBranch ^. uuid)
         (leidenBranch ^. createdAt)
         (fromJust $ appContext ^. currentUser)
         leidenBranchCreate)
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
      amsterdamBranch
      (amsterdamBranch ^. parentPackageId)
      (amsterdamBranch ^. parentPackageId)
      (Just $ userAlbert ^. uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "KM_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
