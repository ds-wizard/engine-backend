module Specs.API.BranchAPISpec where

import Control.Lens
import Data.Aeson
import Data.Either
import Data.Maybe
import qualified Data.UUID as U
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchWithStateDTO
import Common.Error
import Common.Localization
import Database.DAO.Branch.BranchDAO
import Database.DAO.Package.PackageDAO
import Database.Migration.Package.Data.Package
import qualified Database.Migration.Package.PackageMigration as PKG
import LensesConfig
import Model.Branch.BranchState
import Service.Branch.BranchService

import Specs.API.Common
import Specs.Common

branchAPI appContext = do
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. config
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
                { _branchWithStateDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchWithStateDTOName = "Amsterdam KM"
                , _branchWithStateDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchWithStateDTOKmId = "amsterdam-km"
                , _branchWithStateDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchWithStateDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchWithStateDTOState = BSDefault
                }
          let expBody = encode [expDto]
          runInContextIO (deletePackageById (elixirNlPackage2Dto ^. pId)) appContext
          let branch =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          runInContextIO (createBranch branch) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_PERM"
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
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqBody = encode reqDto
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |] "name"
        it "HTTP 400 BAD REQUEST when kmId is not in valid format" $ do
          let reqDto =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam.km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          runInContextIO (createBranch reqDto) appContext
          let reqBody = encode (reqDto & kmId .~ "amsterdam.km-")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when kmId is already taken" $ do
          let reqDto =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqBody = encode reqDto
          runInContextIO (createBranch reqDto) appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS $ reqDto ^. kmId)
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
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:9.9.9"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqBody = encode reqDto
          runInContextIO (createBranch reqDto) appContext
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
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_PERM"
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
                { _branchWithStateDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchWithStateDTOName = "Amsterdam KM"
                , _branchWithStateDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchWithStateDTOKmId = "amsterdam-km"
                , _branchWithStateDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchWithStateDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchWithStateDTOState = BSDefault
                }
          let branch =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          runInContextIO (createBranch branch) appContext
          runInContextIO (deletePackageById (elixirNlPackage2Dto ^. pId)) appContext
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_PERM"
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
              { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
              , _branchDTOName = "EDITED: Amsterdam KM"
              , _branchDTOOrganizationId = "elixir.nl.amsterdam"
              , _branchDTOKmId = "amsterdam-km"
              , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
              , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
              }
        let reqBody = encode reqDto
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          runInContextIO (createBranch expDto) appContext
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. uuid) `shouldBe` (reqDto ^. uuid)
          liftIO $ (branchFromDb ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (branchFromDb ^. kmId) `shouldBe` (reqDto ^. kmId)
          liftIO $ (branchFromDb ^. parentPackageId) `shouldBe` (reqDto ^. parentPackageId)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |] "name"
        it "HTTP 400 BAD REQUEST when kmId is not in valid format" $ do
          let reqDto =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          runInContextIO (createBranch reqDto) appContext
          let reqBody = encode (reqDto & kmId .~ "amsterdam.km")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when kmId is already taken" $ do
          let reqDto =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqDto2 =
                BranchDTO
                { _branchDTOUuid = (fromJust (U.fromString "a0cb5aec-5977-44fc-bd87-8cc1ddf5de6a"))
                , _branchDTOName = "Amsterdam KM 2"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km-2"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          runInContextIO (createBranch reqDto) appContext
          runInContextIO (createBranch reqDto2) appContext
          let reqBody = encode (reqDto & kmId .~ "amsterdam-km-2")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS "amsterdam-km-2")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_PERM"
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
                { _branchDTOUuid = (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          runInContextIO (createBranch branchDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` False
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
