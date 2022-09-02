module Wizard.Specs.API.Package.Detail_GET
  ( detail_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Coordinate
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /packages/{pkgId}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith ((), Application)
detail_get appContext =
  describe "GET /packages/{pkgId}" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT pkgId = BS.pack $ "/packages/" ++ pkgId

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (with token)" appContext [reqAuthHeader] (globalPackage ^. pId)
  create_test_200
    "HTTP 200 OK (with token)"
    appContext
    [reqAuthHeader]
    (buildCoordinate (globalPackage ^. organizationId) (globalPackage ^. kmId) "latest")
  create_test_200 "HTTP 200 OK (without token)" appContext [] (globalPackage ^. pId)
  create_test_200
    "HTTP 200 OK latest (without token)"
    appContext
    []
    (buildCoordinate (globalPackage ^. organizationId) (globalPackage ^. kmId) "latest")

create_test_200 title appContext authHeader pkgId =
  it title $
    -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
    let reqUrl = reqUrlT pkgId
    -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto =
          toDetailDTO
            (toPackage globalPackage)
            [globalRegistryPackage]
            [globalRegistryOrganization, nlRegistryOrganization]
            ["0.0.1", "1.0.0"]
            ("https://registry-test.ds-wizard.org/knowledge-models/" ++ (globalPackage ^. pId))
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
    runInContextIO R.runMigration appContext
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
  createNoPermissionTest appContext reqMethod (reqUrlT (netherlandsPackage ^. pId)) [reqCtHeader] reqBody "PM_READ_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/packages/global:non-existing-package:1.0.0"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "package"
    [("id", "global:non-existing-package:1.0.0")]
