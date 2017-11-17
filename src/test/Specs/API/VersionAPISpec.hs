module Specs.API.VersionAPISpec where

import Control.Lens
import Data.Aeson
import Data.Aeson (Value(..), object, (.=))
import Data.ByteString.Lazy
import Data.Either
import Data.Foldable
import Data.Maybe
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher
import qualified Web.Scotty as S

import Api.Resources.Package.PackageDTO
import Api.Resources.Version.VersionDTO
import Common.Error
import qualified
       Database.Migration.KnowledgeModel.KnowledgeModelContainerMigration
       as KMC
import qualified Database.Migration.Package.PackageMigration as PKG
import Service.Package.PackageService

import Specs.API.Common

versionAPI context dspConfig =
  with (startWebApp context dspConfig) $ do
    describe "VERSION API Spec" $
      -- ------------------------------------------------------------------------
      -- PUT /kmcs/{kmcUuid}/versions/{version}
      -- ------------------------------------------------------------------------
     do
      describe "PUT /kmcs/{kmcUuid}/versions/{version}" $ do
        let reqMethod = methodPut
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/versions/1.0.0"
        it "HTTP 201 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = VersionDTO {_vdtoDescription = "Second Release"}
          let reqBody = encode reqDto
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          eitherParentPackage <-
            liftIO $ getPackageById context "elixir.nl:core-nl:1.0.0"
          liftIO $ (isRight eitherParentPackage) `shouldBe` True
          let (Right parentPackage) = eitherParentPackage
          let expDto =
                PackageDTO
                { _pkgdtoId = "elixir.nl.amsterdam:amsterdam-km:1.0.0"
                , _pkgdtoName = "Amsterdam KM"
                , _pkgdtoGroupId = "elixir.nl.amsterdam"
                , _pkgdtoArtifactId = "amsterdam-km"
                , _pkgdtoVersion = "1.0.0"
                , _pkgdtoDescription = reqDto ^. vdtoDescription
                , _pkgdtoParentPackage = Just parentPackage
                }
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherPackageFromDb <-
            liftIO $
            liftIO $
            getPackageById context "elixir.nl.amsterdam:amsterdam-km:1.0.0"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherPackageFromDb) `shouldBe` True
          let (Right packageFromDb) = eitherPackageFromDb
          liftIO $ packageFromDb `shouldBe` expDto
        it "HTTP 400 BAD REQUEST when version is not in valid format" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = VersionDTO {_vdtoDescription = "Second Release"}
          let reqBody = encode reqDto
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          eitherParentPackage <-
            liftIO $ getPackageById context "elixir.nl:core-nl:1.0.0"
          liftIO $ (isRight eitherParentPackage) `shouldBe` True
          let (Right parentPackage) = eitherParentPackage
          let expDto =
                createErrorWithErrorMessage $ "Version is not in valid format"
          let expBody = encode expDto
          -- WHEN: Call API
          response <-
            request
              reqMethod
              "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/versions/.0.0"
              reqHeaders
              reqBody
          -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when version is not higher than previous one" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = VersionDTO {_vdtoDescription = "Second Release"}
          let reqBody = encode reqDto
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          liftIO $
            createPackageFromKMC
              context
              "6474b24b-262b-42b1-9451-008e8363f2b6"
              "1.0.0"
              "Desc"
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          eitherParentPackage <-
            liftIO $ getPackageById context "elixir.nl:core-nl:1.0.0"
          liftIO $ (isRight eitherParentPackage) `shouldBe` True
          let (Right parentPackage) = eitherParentPackage
          let expDto =
                createErrorWithErrorMessage $
                "New version has to be higher than the previous one"
          let expBody = encode expDto
          -- WHEN: Call API
          response <-
            request
              reqMethod
              "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/versions/0.9.0"
              reqHeaders
              reqBody
          -- THEN: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "description"
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest
          dspConfig
          reqMethod
          reqUrl
          []
          ""
          "KM_PUBLISH_PERM"
