module Wizard.Specs.API.KnowledgeModel.Preview_POST (
  preview_post,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Coordinate
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /knowledge-models/preview
-- ------------------------------------------------------------------------
preview_post :: AppContext -> SpecWith ((), Application)
preview_post appContext =
  describe "POST /knowledge-models/preview" $ do
    test_200 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/knowledge-models/preview"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT pkgId =
  KnowledgeModelChangeDTO
    { packageId = Just pkgId
    , events = []
    , tagUuids = []
    }

reqBodyT pkg = encode (reqDtoT pkg)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (with token)" appContext [reqAuthHeader] germanyPackage germanyPackage.pId km1WithQ4
  create_test_200
    "HTTP 200 OK (with token)"
    appContext
    [reqAuthHeader]
    germanyPackage
    (buildCoordinate germanyPackage.organizationId germanyPackage.kmId "latest")
    km1WithQ4
  create_test_200 "HTTP 200 OK (without token)" appContext [] globalPackage globalPackage.pId km1Global
  create_test_200
    "HTTP 200 OK (without token)"
    appContext
    []
    globalPackage
    (buildCoordinate globalPackage.organizationId globalPackage.kmId "latest")
    km1Global

create_test_200 title appContext authHeader pkg pkgId expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT pkgId
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO PKG.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (reqBodyT germanyPackage.pId) "QTN_PERM"
