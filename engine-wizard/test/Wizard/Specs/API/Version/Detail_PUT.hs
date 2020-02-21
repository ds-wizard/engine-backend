module Wizard.Specs.API.Version.Detail_PUT
  ( detail_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Version.VersionDTO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Version.VersionService

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Package.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /branches/{branchUuid}/versions/{version}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith Application
detail_put appContext =
  describe "PUT /branches/{branchUuid}/versions/{version}" $ do
    test_201 appContext
    test_400_invalid_json appContext
    test_400_invalid_version_format appContext
    test_400_not_higher_pkg_version appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/versions/1.0.0"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto =
  VersionDTO
    { _versionDTODescription = amsterdamPackage ^. description
    , _versionDTOReadme = amsterdamPackage ^. readme
    , _versionDTOLicense = amsterdamPackage ^. license
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toSimpleDTO . toPackage $ amsterdamPackage
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
    runInContextIO B.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, PackageSimpleDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    comparePackageDtos resBody expDto
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfPackageInDB appContext expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "description"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_version_format appContext = do
  it "HTTP 400 BAD REQUEST when version is not in a valid format" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/versions/.0.0"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createUserError _ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT
    let expBody = encode expDto
     -- AND: Run migrations
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
test_400_not_higher_pkg_version appContext = do
  it "HTTP 400 BAD REQUEST when version is not higher than the previous one" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createUserError _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
    runInContextIO B.runMigration appContext
    runInContextIO (publishPackage "6474b24b-262b-42b1-9451-008e8363f2b6" "1.0.0" reqDto) appContext
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
test_403 appContext =
  createNoPermissionTest (appContext ^. applicationConfig) reqMethod reqUrl [reqCtHeader] reqBody "KM_PUBLISH_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/versions/1.0.0"
    reqHeaders
    reqBody
    "branch"
    "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
