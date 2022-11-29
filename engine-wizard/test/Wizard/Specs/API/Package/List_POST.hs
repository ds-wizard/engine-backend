module Wizard.Specs.API.Package.List_POST (
  list_post,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Api.Resource.PackageBundle.PackageBundleJM ()
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.PackageBundle.Data.PackageBundles
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle
import Shared.Service.Package.PackageMapper
import qualified Shared.Service.PackageBundle.PackageBundleMapper as PBM
import Shared.Util.String (replace)
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Package.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /packages
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext =
  describe "POST /packages" $ do
    test_201_req_all_db_all appContext
    test_201_req_all_db_no appContext
    test_201_req_no_db_all appContext
    test_201_req_one_db_rest appContext
    test_201_without_readme appContext
    test_400 appContext
    test_400_main_package_duplication appContext
    test_400_missing_previous_package appContext
    test_400_bad_package_coordinates appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = PBM.toDTO netherlandsPackageV2Budle

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_all_db_all appContext =
  it "HTTP 201 CREATED - In request: all previous packages, in DB: all previous packages" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toSimpleDTO . toPackage <$> [netherlandsPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      runInContextIO (insertPackage netherlandsPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
      assertExistenceOfPackageInDB appContext (head reqDto.packages)
      assertExistenceOfPackageInDB appContext (reqDto.packages !! 1)
      assertExistenceOfPackageInDB appContext (reqDto.packages !! 2)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_all_db_no appContext =
  it "HTTP 201 CREATED - In request: all previous packages, in DB: no previous packages" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toSimpleDTO . toPackage <$> [globalPackage, netherlandsPackage, netherlandsPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
      assertExistenceOfPackageInDB appContext globalPackage
      assertExistenceOfPackageInDB appContext netherlandsPackage
      assertExistenceOfPackageInDB appContext netherlandsPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_no_db_all appContext =
  it "HTTP 201 CREATED - In request: no previous packages, in DB: all previous packages" $
    -- GIVEN: Prepare request
    do
      let reqDto = PBM.toDTO (netherlandsPackageV2Budle {packages = [netherlandsPackageV2]})
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toSimpleDTO . toPackage <$> [netherlandsPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      runInContextIO (insertPackage netherlandsPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
      assertExistenceOfPackageInDB appContext globalPackage
      assertExistenceOfPackageInDB appContext netherlandsPackage
      assertExistenceOfPackageInDB appContext netherlandsPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_one_db_rest appContext =
  it "HTTP 201 CREATED - In request: one previous package, in DB: rest of previous packages" $
    -- GIVEN: Prepare request
    do
      let reqDto = PBM.toDTO (netherlandsPackageV2Budle {packages = [netherlandsPackage, netherlandsPackageV2]})
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toSimpleDTO . toPackage <$> [netherlandsPackage, netherlandsPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
      assertExistenceOfPackageInDB appContext globalPackage
      assertExistenceOfPackageInDB appContext netherlandsPackage
      assertExistenceOfPackageInDB appContext netherlandsPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_without_readme appContext =
  it "HTTP 201 CREATED - Without 'readme' props" $
    -- GIVEN: Prepare request
    do
      let reqDto = PBM.toDTO (netherlandsPackageBudle {packages = [netherlandsPackage]})
      let reqBody = BSL.pack . replace "readme" "differentReadme" . BSL.unpack $ encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toSimpleDTO . toPackage <$> [netherlandsPackage {readme = ""} :: PackageWithEvents]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 2
      assertExistenceOfPackageInDB appContext (head expDto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqBody = BSL.pack "{}"
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError . _ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT $ "packages"
    let expBody = encode expDto
    -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_main_package_duplication appContext =
  it "HTTP 400 BAD REQUEST when main package already exists" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS netherlandsPackageV2.pId
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      runInContextIO (insertPackage netherlandsPackage) appContext
      runInContextIO (insertPackage netherlandsPackageV2) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3

test_400_missing_previous_package appContext =
  it "HTTP 400 BAD REQUEST when missing previous package" $
    -- GIVEN: Prepare request
    do
      let reqDto = PBM.toDTO (netherlandsPackageV2Budle {packages = [netherlandsPackageV2]})
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            UserError $
              _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST netherlandsPackage.pId netherlandsPackageV2.pId
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 1
      assertExistenceOfPackageInDB appContext globalPackage

test_400_bad_package_coordinates appContext =
  it "HTTP 400 BAD REQUEST when package ID doesn't match with package coordinates" $
    -- GIVEN: Prepare request
    do
      let editedElixirNlPackageDto = netherlandsPackage {kmId = netherlandsPackage.kmId ++ "-2"} :: PackageWithEvents
      let reqDto = PBM.toDTO (netherlandsPackageV2Budle {packages = [editedElixirNlPackageDto, netherlandsPackageV2]})
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__COORDINATE_MISMATCH netherlandsPackage.pId
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PM_WRITE_PERM"
