module Specs.API.Branch.Detail_DELETE
  ( detail_delete
  ) where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Branch.BranchDAO
import Database.Migration.Development.Branch.Data.Branches
import LensesConfig
import Model.Context.AppContext
import Service.Branch.BranchService

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /branches/{branchId}
-- ------------------------------------------------------------------------
detail_delete :: AppContext -> SpecWith Application
detail_delete appContext =
  describe "DELETE /branches/{branchId}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  it "HTTP 204 NO CONTENT" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
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
    assertCountInDB findBranches appContext 0

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
