module Specs.API.PackageAPISpec where

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
import Database.DAO.Package.PackageDAO
import qualified Database.Migration.Package.PackageMigration as PKG
import Model.Package.Package
import Service.Package.PackageService

import Specs.API.Common

packageAPI context dspConfig = do
  let dto1 =
        PackageDTO
        { _pkgdtoId = "elixir-base:0.0.1"
        , _pkgdtoName = "Elixir Base"
        , _pkgdtoGroupId = "elixir.base"
        , _pkgdtoArtifactId = "core"
        , _pkgdtoVersion = "0.0.1"
        , _pkgdtoDescription = "Beta version"
        , _pkgdtoParentPackage = Nothing
        }
  let dto2 =
        PackageDTO
        { _pkgdtoId = "elixir-base:1.0.0"
        , _pkgdtoName = "Elixir Base"
        , _pkgdtoGroupId = "elixir.base"
        , _pkgdtoArtifactId = "core"
        , _pkgdtoVersion = "1.0.0"
        , _pkgdtoDescription = "First Release"
        , _pkgdtoParentPackage = Nothing
        }
  let dto3 =
        PackageDTO
        { _pkgdtoId = "elixir-nl:1.0.0"
        , _pkgdtoName = "Elixir Netherlands"
        , _pkgdtoGroupId = "elixir.nl"
        , _pkgdtoArtifactId = "core-nl"
        , _pkgdtoVersion = "1.0.0"
        , _pkgdtoDescription = "First Release"
        , _pkgdtoParentPackage = Just dto2
        }
  with (startWebApp context dspConfig) $ do
    describe "PACKAGE API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /packages
      -- ------------------------------------------------------------------------
     do
      do describe "GET /packages" $ do
           let reqMethod = methodGet
           let reqUrl = "/packages"
           it "HTTP 200 OK" $
          -- GIVEN: Prepare request
            do
             let reqHeaders = [reqAuthHeader, reqCtHeader]
             liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
             let expStatus = 200
             let expHeaders = [resCtHeader] ++ resCorsHeaders
             let expDto = [dto1, dto2, dto3]
             let expBody = encode expDto
          -- WHEN: Call API
             response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expetation
             let responseMatcher =
                   ResponseMatcher
                   { matchHeaders = expHeaders
                   , matchStatus = expStatus
                   , matchBody = bodyEquals expBody
                   }
             response `shouldRespondWith` responseMatcher
           createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- GET /packages/{packageName}
      -- ------------------------------------------------------------------------
         describe "GET /packages/{packageName}" $ do
           let reqMethod = methodGet
           let reqUrl = "/packages/elixir-base"
           it "HTTP 200 OK" $
          -- GIVEN: Prepare request
            do
             let reqHeaders = [reqAuthHeader, reqCtHeader]
             liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
             let expStatus = 200
             let expHeaders = [resCtHeader] ++ resCorsHeaders
             let expDto = [dto1, dto2]
             let expBody = encode expDto
          -- WHEN: Call API
             response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expetation
             let responseMatcher =
                   ResponseMatcher
                   { matchHeaders = expHeaders
                   , matchStatus = expStatus
                   , matchBody = bodyEquals expBody
                   }
             response `shouldRespondWith` responseMatcher
           createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- DELETE /packages/{packageName}
      -- ------------------------------------------------------------------------
         describe "DELETE /packages/{packageName}" $ do
           let reqMethod = methodDelete
           let reqUrl = "/packages/elixir-base"
           it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare request
            do
             let reqHeaders = [reqAuthHeader, reqCtHeader]
             liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
             let expStatus = 204
             let expHeaders = resCorsHeaders
          -- WHEN: Call API
             response <- request reqMethod reqUrl reqHeaders ""
          -- THEN: Find a result
             packages <- liftIO $ findPackagesByArtifactId context "elixir-base"
          -- AND: Compare response with expetation
             let responseMatcher =
                   ResponseMatcher
                   { matchHeaders = expHeaders
                   , matchStatus = expStatus
                   , matchBody = bodyEquals ""
                   }
             response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
             liftIO $ packages `shouldBe` []
           createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- DELETE /packages/{packageName}/versions/{version}
      -- ------------------------------------------------------------------------
         describe "DELETE /packages/{packageName}/versions/{version}" $ do
           let reqMethod = methodDelete
           let reqUrl = "/packages/elixir-base/versions/1.0.0"
           it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare request
            do
             let reqHeaders = [reqAuthHeader, reqCtHeader]
             liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
             let expStatus = 204
             let expHeaders = resCorsHeaders
          -- WHEN: Call API
             response <- request reqMethod reqUrl reqHeaders ""
          -- THEN: Find a result
             packages <- liftIO $ getPackagesForName context "elixir-base"
          -- AND: Compare response with expetation
             let responseMatcher =
                   ResponseMatcher
                   { matchHeaders = expHeaders
                   , matchStatus = expStatus
                   , matchBody = bodyEquals ""
                   }
             response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
             liftIO $ packages `shouldBe` [dto1]
           createAuthTest reqMethod reqUrl [] ""
