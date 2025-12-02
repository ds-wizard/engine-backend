module Wizard.Specs.API.KnowledgeModelPackage.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String (replace)
import Shared.Coordinate.Localization.Messages.Public
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Localization.Messages.Public
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelPackage.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-packages
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/knowledge-model-packages" $ do
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

reqUrl = "/wizard-api/knowledge-model-packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = netherlandsV2KmBundle

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
      let expDto = fmap toSimpleDTO [netherlandsKmPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
      runInContextIO (insertPackage netherlandsKmPackage) appContext
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
      let expDto = fmap toSimpleDTO [globalKmPackage, netherlandsKmPackage, netherlandsKmPackageV2]
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
      assertExistenceOfPackageInDB appContext globalKmPackage
      assertExistenceOfPackageInDB appContext netherlandsKmPackage
      assertExistenceOfPackageInDB appContext netherlandsKmPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_no_db_all appContext =
  it "HTTP 201 CREATED - In request: no previous packages, in DB: all previous packages" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toSimpleDTO [netherlandsKmPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
      runInContextIO (insertPackage netherlandsKmPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
      assertExistenceOfPackageInDB appContext globalKmPackage
      assertExistenceOfPackageInDB appContext netherlandsKmPackage
      assertExistenceOfPackageInDB appContext netherlandsKmPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_one_db_rest appContext =
  it "HTTP 201 CREATED - In request: one previous package, in DB: rest of previous packages" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsKmBundlePackage, netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toSimpleDTO [netherlandsKmPackage, netherlandsKmPackageV2]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
      assertExistenceOfPackageInDB appContext globalKmPackage
      assertExistenceOfPackageInDB appContext netherlandsKmPackage
      assertExistenceOfPackageInDB appContext netherlandsKmPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_without_readme appContext =
  it "HTTP 201 CREATED - Without 'readme' field" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsKmBundlePackage, netherlandsV2KmBundlePackage]}
      let reqBody = BSL.pack . replace "readme" "differentReadme" . BSL.unpack $ encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toSimpleDTO [netherlandsKmPackage {readme = ""} :: KnowledgeModelPackage, netherlandsKmPackageV2 {readme = ""} :: KnowledgeModelPackage]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 3
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
      let expDto = UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS netherlandsKmPackageV2.pId
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
      runInContextIO (insertPackage netherlandsKmPackage) appContext
      runInContextIO (insertPackage netherlandsKmPackageV2) appContext
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
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            UserError $
              _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST netherlandsKmPackage.pId netherlandsKmPackageV2.pId
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages appContext 1
      assertExistenceOfPackageInDB appContext globalKmPackage

test_400_bad_package_coordinates appContext =
  it "HTTP 400 BAD REQUEST when package ID doesn't match with package coordinates" $
    -- GIVEN: Prepare request
    do
      let editedElixirNlPackageDto = netherlandsKmBundlePackage {kmId = netherlandsKmPackage.kmId ++ "-2"} :: KnowledgeModelBundlePackage
      let reqDto = netherlandsV2KmBundle {packages = [editedElixirNlPackageDto, netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__COORDINATE_MISMATCH netherlandsKmPackage.pId
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages appContext
      runInContextIO (insertPackage globalKmPackage) appContext
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
