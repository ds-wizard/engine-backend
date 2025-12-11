module Wizard.Specs.API.KnowledgeModel.Preview_POST (
  preview_POST,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-models/preview
-- ------------------------------------------------------------------------
preview_POST :: AppContext -> SpecWith ((), Application)
preview_POST appContext =
  describe "POST /wizard-api/knowledge-models/preview" $ do
    test_200 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-models/preview"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT pkgId =
  KnowledgeModelChangeDTO
    { knowledgeModelPackageId = Just pkgId
    , events = []
    , tagUuids = []
    }

reqBodyT pkg = encode (reqDtoT pkg)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (with token)" appContext [reqAuthHeader] germanyKmPackage germanyKmPackage.pId km1WithQ4
  create_test_200
    "HTTP 200 OK (with token)"
    appContext
    [reqAuthHeader]
    germanyKmPackage
    (buildCoordinate germanyKmPackage.organizationId germanyKmPackage.kmId "latest")
    km1WithQ4
  create_test_200 "HTTP 200 OK (without token)" appContext [] globalKmPackage globalKmPackage.pId km1Global
  create_test_200
    "HTTP 200 OK (without token)"
    appContext
    []
    globalKmPackage
    (buildCoordinate globalKmPackage.organizationId globalKmPackage.kmId "latest")
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
      runInContextIO KnowledgeModelPackage.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
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
  createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (reqBodyT germanyKmPackage.pId) "PRJ_PERM"
