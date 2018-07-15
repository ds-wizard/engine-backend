module Specs.API.VersionAPISpec where

import Control.Lens
import Data.Aeson
import Data.Either
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Package.PackageDTO
import Api.Resource.Version.VersionDTO
import qualified
       Database.Migration.Development.Branch.BranchMigration as B
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import LensesConfig
import Localization
import Model.Error.ErrorHelpers
import Service.Package.PackageService

import Specs.API.Common
import Specs.Common

versionAPI appContext =
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. config
    describe "VERSION API Spec" $
      -- ------------------------------------------------------------------------
      -- PUT /branches/{branchUuid}/versions/{version}
      -- ------------------------------------------------------------------------
     do
      describe "PUT /branches/{branchUuid}/versions/{version}" $ do
        let reqMethod = methodPut
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/versions/1.0.0"
        it "HTTP 201 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = VersionDTO {_versionDTODescription = "Second Release"}
          let reqBody = encode reqDto
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          eitherParentPackage <- runInContextIO (getPackageById "elixir.nl:core-nl:1.0.0") appContext
          liftIO $ (isRight eitherParentPackage) `shouldBe` True
          let (Right parentPackage) = eitherParentPackage
          let expDto =
                PackageDTO
                { _packageDTOPId = "elixir.nl.amsterdam:amsterdam-km:1.0.0"
                , _packageDTOName = "Amsterdam KM"
                , _packageDTOOrganizationId = "elixir.nl.amsterdam"
                , _packageDTOKmId = "amsterdam-km"
                , _packageDTOVersion = "1.0.0"
                , _packageDTODescription = reqDto ^. description
                , _packageDTOParentPackageId = Just $ parentPackage ^. pId
                }
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackageFromDb <- runInContextIO (getPackageById "elixir.nl.amsterdam:amsterdam-km:1.0.0") appContext
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackageFromDb) `shouldBe` True
          let (Right packageFromDb) = eitherPackageFromDb
          liftIO $ packageFromDb `shouldBe` expDto
        it "HTTP 400 BAD REQUEST when version is not in valid format" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = VersionDTO {_versionDTODescription = "Second Release"}
          let reqBody = encode reqDto
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          eitherParentPackage <- runInContextIO (getPackageById "elixir.nl:core-nl:1.0.0") appContext
          liftIO $ (isRight eitherParentPackage) `shouldBe` True
          let (Right parentPackage) = eitherParentPackage
          let expDto = createErrorWithErrorMessage $ _ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT
          let expBody = encode expDto
          -- WHEN: Call API
          response <-
            request reqMethod "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/versions/.0.0" reqHeaders reqBody
          -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when version is not higher than previous one" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = VersionDTO {_versionDTODescription = "Second Release"}
          let reqBody = encode reqDto
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          let versionDto = VersionDTO {_versionDTODescription = "Desc"}
          runInContextIO (createPackageFromKMC "6474b24b-262b-42b1-9451-008e8363f2b6" "1.0.0" versionDto) appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          eitherParentPackage <- runInContextIO (getPackageById "elixir.nl:core-nl:1.0.0") appContext
          liftIO $ (isRight eitherParentPackage) `shouldBe` True
          let (Right parentPackage) = eitherParentPackage
          let expDto = createErrorWithErrorMessage _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
          let expBody = encode expDto
          -- WHEN: Call API
          response <-
            request reqMethod "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/versions/0.9.0" reqHeaders reqBody
          -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "description"
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_PUBLISH_PERM"
