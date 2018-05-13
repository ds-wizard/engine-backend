module Specs.API.PackageAPISpec where

import Control.Lens
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import Data.Either
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Package.PackageDTO
import Common.Error
import Database.DAO.Branch.BranchDAO
import Database.DAO.Package.PackageDAO
import qualified Database.Migration.Branch.BranchMigration as B
import qualified Database.Migration.Package.PackageMigration as PKG
import LensesConfig
import Service.Package.PackageMapper
import Service.Package.PackageService

import Specs.API.Common

packageAPI appContext = do
  let dto1 =
        PackageDTO
        { _packageDTOPId = "elixir.base:core:0.0.1"
        , _packageDTOName = "Elixir Base Package"
        , _packageDTOOrganizationId = "elixir.base"
        , _packageDTOArtifactId = "core"
        , _packageDTOVersion = "0.0.1"
        , _packageDTODescription = "Beta version"
        , _packageDTOParentPackageId = Nothing
        }
  let dto2 =
        PackageDTO
        { _packageDTOPId = "elixir.base:core:1.0.0"
        , _packageDTOName = "Elixir Base Package"
        , _packageDTOOrganizationId = "elixir.base"
        , _packageDTOArtifactId = "core"
        , _packageDTOVersion = "1.0.0"
        , _packageDTODescription = "First Release"
        , _packageDTOParentPackageId = Nothing
        }
  let dto3 =
        PackageDTO
        { _packageDTOPId = "elixir.nl:core-nl:1.0.0"
        , _packageDTOName = "Elixir Netherlands"
        , _packageDTOOrganizationId = "elixir.nl"
        , _packageDTOArtifactId = "core-nl"
        , _packageDTOVersion = "1.0.0"
        , _packageDTODescription = "First Release"
        , _packageDTOParentPackageId = Just $ dto2 ^. pId
        }
  let dto4 =
        PackageDTO
        { _packageDTOPId = "elixir.nl:core-nl:2.0.0"
        , _packageDTOName = "Elixir Netherlands"
        , _packageDTOOrganizationId = "elixir.nl"
        , _packageDTOArtifactId = "core-nl"
        , _packageDTOVersion = "2.0.0"
        , _packageDTODescription = "Second Release"
        , _packageDTOParentPackageId = Just $ dto3 ^. pId
        }
  with (startWebApp appContext) $ do
    let context = appContext ^. oldContext
    let dswConfig = appContext ^. config
    describe "PACKAGE API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /packages
      -- ------------------------------------------------------------------------
     do
      describe "GET /packages" $
         -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/packages"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = [dto1, dto2, dto3, dto4]
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "PM_PERM"
      -- ------------------------------------------------------------------------
      -- GET /packages?organizationId={organizationId}&artifactId={artifactId}
      -- ------------------------------------------------------------------------
      describe "GET /packages?organizationId={organizationId}&artifactId={artifactId}" $
          -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/packages?organizationId=elixir.base&artifactId=core"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
        -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = [dto1, dto2]
          let expBody = encode expDto
        -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
        -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "PM_PERM"
      -- ------------------------------------------------------------------------
      -- GET /packages/{pkgId}
      -- ------------------------------------------------------------------------
      describe "GET /packages/{pkgId}" $
         -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/packages/elixir.base:core:1.0.0"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
           -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = dto2
          let expBody = encode expDto
           -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
           -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "PM_PERM"
        createNotFoundTest reqMethod "/packages/elixir.nonexist:nopackage:2.0.0" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- DELETE /packages
      -- ------------------------------------------------------------------------
      describe "DELETE /packages" $
         -- GIVEN: Prepare request
       do
        let reqMethod = methodDelete
        let reqUrl = "/packages"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO $ deleteBranches context
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackages <- liftIO $ findPackages context
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackages) `shouldBe` True
          let (Right packages) = eitherPackages
          liftIO $ packages `shouldBe` []
        it "HTTP 400 BAD REQUEST when package can't be deleted" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ B.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = resCorsHeaders
          let expDto =
                createErrorWithErrorMessage $
                "Package 'elixir.nl:core-nl:1.0.0' can't be deleted. It's used by some branch."
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackages <- liftIO $ findPackages context
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackages) `shouldBe` True
          let (Right packages) = eitherPackages
          liftIO $ packages `shouldBe` [fromDTO dto1, fromDTO dto2, fromDTO dto3, fromDTO dto4]
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "PM_PERM"
      -- ------------------------------------------------------------------------
      -- DELETE /packages?organizationId={organizationId}&artifactId={artifactId}
      -- ------------------------------------------------------------------------
      describe "DELETE /packages?organizationId={organizationId}&artifactId={artifactId}" $
         -- GIVEN: Prepare request
       do
        let reqMethod = methodDelete
        let reqUrl = "/packages?organizationId=elixir.nl&artifactId=core-nl"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- AND: Prepare DB
          liftIO $ deleteBranches context
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackages <- liftIO $ findPackageByOrganizationIdAndArtifactId context "elixir.nl" "core-nl"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackages) `shouldBe` True
          let (Right packages) = eitherPackages
          liftIO $ packages `shouldBe` []
        it "HTTP 400 BAD REQUEST when package can't be deleted" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ B.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = resCorsHeaders
          let expDto =
                createErrorWithErrorMessage $
                "Package 'elixir.nl:core-nl:1.0.0' can't be deleted. It's used by some branch."
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackages <- liftIO $ findPackages context
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackages) `shouldBe` True
          let (Right packages) = eitherPackages
          liftIO $ packages `shouldBe` [fromDTO dto1, fromDTO dto2, fromDTO dto3, fromDTO dto4]
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "PM_PERM"
      -- ------------------------------------------------------------------------
      -- DELETE /packages/{pkgId}
      -- ------------------------------------------------------------------------
      describe "DELETE /packages/{pkgId}" $
         -- GIVEN: Prepare request
       do
        let reqMethod = methodDelete
        let reqUrl = "/packages/elixir.nl:core-nl:2.0.0"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
        -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
        -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
        -- THEN: Find a result
          eitherPackage <- liftIO $ getPackageById context "elixir.nl:core-nl:2.0.0"
        -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
        -- AND: Compare state in DB with expetation
          liftIO $ (isLeft eitherPackage) `shouldBe` True
          let (Left (NotExistsError _)) = eitherPackage
        -- AND: We have to end with expression (if there is another way, how to do it, please fix it)
          liftIO $ True `shouldBe` True
        it "HTTP 400 BAD REQUEST when package can't be deleted" $
          -- GIVEN: Prepare request
         do
          let reqUrl = "/packages/elixir.nl:core-nl:1.0.0"
          -- AND: Prepare DB
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ B.runMigration appContext
          -- AND: Prepare expectation
          let expStatus = 400
          let expHeaders = resCorsHeaders
          let expDto =
                createErrorWithErrorMessage $
                "Package 'elixir.nl:core-nl:1.0.0' can't be deleted. It's used by some branch."
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackages <- liftIO $ findPackages context
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackages) `shouldBe` True
          let (Right packages) = eitherPackages
          liftIO $ packages `shouldBe` [fromDTO dto1, fromDTO dto2, fromDTO dto3, fromDTO dto4]
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "PM_PERM"
        createNotFoundTest reqMethod "/packages/elixir.nonexist:nopackage:2.0.0" reqHeaders reqBody
