module Specs.API.BranchAPISpec where

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

import Api.Resources.Branch.BranchDTO
import Api.Resources.Branch.BranchWithStateDTO
import Common.Error
import Database.DAO.Branch.BranchDAO
import Database.DAO.Package.PackageDAO
import Database.Migration.Package.Data.Package
import qualified Database.Migration.Package.PackageMigration as PKG
import Model.Branch.Branch
import Model.Branch.BranchState
import Model.Package.Package
import Service.Branch.BranchService

import Specs.API.Common
import Specs.Common

branchAPI context dspConfig = do
  with (startWebApp context dspConfig) $ do
    describe "BRANCH API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /branches
      -- ------------------------------------------------------------------------
     do
      describe "GET /branches" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/branches"
        it "HTTP 200 OK" $ do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- AND: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                BranchWithStateDTO
                { _bwsdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bwsdtoName = "Amsterdam KM"
                , _bwsdtoArtifactId = "amsterdam-km"
                , _bwsdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _bwsdtoState = BSDefault
                }
          let expBody = encode [expDto]
          liftIO $ deletePackageById context (elixirNlPackage2Dto ^. pkgweId)
          let branch =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createBranch context branch
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
      -- ------------------------------------------------------------------------
      -- POST /branches
      -- ------------------------------------------------------------------------
      describe "POST /branches" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodPost
        let reqUrl = "/branches"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        it "HTTP 201 CREATED" $ do
          let reqDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqBody = encode reqDto
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- liftIO $ findBranchById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |] "name"
        it "HTTP 400 BAD REQUEST when artifactId is not in valid format" $ do
          let reqDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam.km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createBranch context reqDto
          let reqBody = encode (reqDto & bdtoArtifactId .~ "amsterdam.km")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("artifactId", "ArtifactId is not in valid format")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when artifactId is already taken" $ do
          let reqDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqBody = encode reqDto
          liftIO $ createBranch context reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("artifactId", "ArtifactId is already taken")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when parentPackageId does not exist" $ do
          let reqDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:9.9.9"
                }
          let reqBody = encode reqDto
          liftIO $ createBranch context reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("parentPackageId", "Parent package doesn't exist")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
      -- ------------------------------------------------------------------------
      -- GET /branches/{branchId}
      -- ------------------------------------------------------------------------
      describe "GET /branches/{branchId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                BranchWithStateDTO
                { _bwsdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bwsdtoName = "Amsterdam KM"
                , _bwsdtoArtifactId = "amsterdam-km"
                , _bwsdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _bwsdtoState = BSDefault
                }
          let branch =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createBranch context branch
          liftIO $ deletePackageById context (elixirNlPackage2Dto ^. pkgweId)
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
       ------------------------------------------------------------------------
       -- PUT /branches/{branchId}
       ------------------------------------------------------------------------
      describe "PUT /branches/{branchId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodPut
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto =
              BranchDTO
              { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
              , _bdtoName = "EDITED: Amsterdam KM"
              , _bdtoArtifactId = "amsterdam-km"
              , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
              }
        let reqBody = encode reqDto
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          liftIO $ createBranch context expDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- liftIO $ findBranchById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. bUuid) `shouldBe` (reqDto ^. bdtoUuid)
          liftIO $ (branchFromDb ^. bName) `shouldBe` (reqDto ^. bdtoName)
          liftIO $ (branchFromDb ^. bArtifactId) `shouldBe` (reqDto ^. bdtoArtifactId)
          liftIO $ (branchFromDb ^. bParentPackageId) `shouldBe` (reqDto ^. bdtoParentPackageId)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |] "name"
        it "HTTP 400 BAD REQUEST when artifactId is not in valid format" $ do
          let reqDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createBranch context reqDto
          let reqBody = encode (reqDto & bdtoArtifactId .~ "amsterdam.km")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("artifactId", "ArtifactId is not in valid format")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when artifactId is already taken" $ do
          let reqDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqDto2 =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "a0cb5aec-5977-44fc-bd87-8cc1ddf5de6a"))
                , _bdtoName = "Amsterdam KM 2"
                , _bdtoArtifactId = "amsterdam-km-2"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createBranch context reqDto
          liftIO $ createBranch context reqDto2
          let reqBody = encode (reqDto & bdtoArtifactId .~ "amsterdam-km-2")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("artifactId", "ArtifactId is already taken")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- DELETE /branches/{branchId}
      -- ------------------------------------------------------------------------
      describe "DELETE /branches/{branchId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodDelete
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- GIVEN: Save KMC to DB
          let branchDto =
                BranchDTO
                { _bdtoUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _bdtoName = "Amsterdam KM"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createBranch context branchDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- liftIO $ findBranchById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` False
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
