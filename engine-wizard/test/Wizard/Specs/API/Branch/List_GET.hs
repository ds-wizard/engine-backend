module Wizard.Specs.API.Branch.List_GET
  ( list_GET
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /branches
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /branches" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/branches"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK" appContext "/branches" (Page "branches" (PageMetadata 20 1 1 0) [amsterdamBranch])
  create_test_200
    "HTTP 200 OK (query)"
    appContext
    "/branches?q=Amsterdam Knowledge Model"
    (Page "branches" (PageMetadata 20 1 1 0) [amsterdamBranch])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/branches?q=Non-existing Branch"
    (Page "branches" (PageMetadata 20 0 0 0) [])

create_test_200 title appContext reqUrl expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO (deletePackageById (netherlandsPackageV2 ^. pId)) appContext
    runInContextIO B.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_PERM"
