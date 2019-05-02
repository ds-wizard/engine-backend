module Specs.API.Package.Detail_DELETE
  ( detail_delete
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Package.PackageDAO
import qualified
       Database.Migration.Development.Branch.BranchMigration as B
import Database.Migration.Development.Package.Data.Packages
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Service.Package.PackageService

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /packages/{pkgId}
-- ------------------------------------------------------------------------
detail_delete :: AppContext -> SpecWith Application
detail_delete appContext =
  describe "DELETE /packages/{pkgId}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/packages/" ++ (netherlandsPackageV2 ^. pId)

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Find a result
    eitherPackage <- runInContextIO (getPackageById $ netherlandsPackageV2 ^. pId) appContext
     -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Compare state in DB with expectation
    liftIO $ (isLeft eitherPackage) `shouldBe` True
    let (Left (NotExistsError _)) = eitherPackage
     -- AND: We have to end with expression (if there is another way, how to do it, please fix it)
    liftIO $ True `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  it "HTTP 400 BAD REQUEST when package can't be deleted" $
    -- GIVEN: Prepare request
   do
    let reqUrl = BS.pack $ "/packages/" ++ (netherlandsPackage ^. pId)
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto =
          createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY
            (netherlandsPackage ^. pId)
            "knowledge model"
    let expBody = encode expDto
    -- AND: Prepare DB
    runInContextIO PKG.runMigration appContext
    runInContextIO B.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Find a result
    eitherPackages <- runInContextIO findPackageWithEvents appContext
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Compare state in DB with expectation
    liftIO $ (isRight eitherPackages) `shouldBe` True
    let (Right packages) = eitherPackages
    liftIO $ packages `shouldBe` [globalPackageEmpty, globalPackage, netherlandsPackage, netherlandsPackageV2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] reqBody "PM_WRITE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = createNotFoundTest reqMethod "/packages/dsw.global:non-existing-package:1.0.0" reqHeaders reqBody
