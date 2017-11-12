module Specs.API.VersionAPISpec where

import Control.Lens
import Data.Aeson
import Data.Aeson (Value(..), object, (.=))
import Data.ByteString.Lazy
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

import Data.Foldable

import Api.Resources.Package.PackageDTO
import Api.Resources.Version.VersionDTO
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
          parentPackage <-
            liftIO $ getPackageById context "elixir.nl:core-nl:1.0.0"
          let expDto =
                PackageDTO
                { _pkgdtoId = "elixir-nl-ams:1.0.0"
                , _pkgdtoName = "Amsterdam KM"
                , _pkgdtoGroupId = "elixir.nl"
                , _pkgdtoArtefactId = "amsterdam-core"
                , _pkgdtoVersion = "1.0.0"
                , _pkgdtoDescription = reqDto ^. vdtoDescription
                , _pkgdtoParentPackage = Just . fromJust $ parentPackage
                }
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          maybePackageFromDb <-
            liftIO $
            liftIO $ getPackageById context "elixir.nl.ams:amsterdam-core:1.0.0"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isJust maybePackageFromDb) `shouldBe` True
          let packageFromDb = fromJust maybePackageFromDb
          liftIO $ packageFromDb `shouldBe` expDto
        createAuthTest reqMethod reqUrl [] ""
