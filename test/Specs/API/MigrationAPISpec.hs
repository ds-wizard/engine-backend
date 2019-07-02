module Specs.API.MigrationAPISpec where

import Control.Lens
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Branch.BranchCreateDTO
import Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Database.DAO.Package.PackageDAO
import qualified
       Database.Migration.Development.Branch.BranchMigration as B
import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.Package.Data.Packages
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import LensesConfig
import Localization
import Model.Error.Error
import Model.Migration.KnowledgeModel.MigratorState
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Migration.KnowledgeModel.MigratorService

import Specs.API.Common
import Specs.Common

timestamp = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0

migratorAPI appContext = do
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. appConfig
    describe "MIGRATOR API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /branches/{branchId}/migrations/current
      -- ------------------------------------------------------------------------
     do
      describe "GET /branches/{branchId}/migrations/current" $
        -- GIVEN: Prepare request
       do
        let branchUuid = "6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqMethod = methodGet
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                MigratorStateDTO
                { _migratorStateDTOBranchUuid = fromJust . U.fromString $ branchUuid
                , _migratorStateDTOMigrationState =
                    ConflictState . CorrectorConflict . Prelude.head $ netherlandsPackageV2 ^. events
                , _migratorStateDTOBranchParentId = netherlandsPackage ^. pId
                , _migratorStateDTOTargetPackageId = netherlandsPackageV2 ^. pId
                , _migratorStateDTOCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1Netherlands
                }
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_UPGRADE_PERM"
        it "HTTP 404 NOT FOUND - migration doesn't exist" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 404
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "kmMigration" "6474b24b-262b-42b1-9451-008e8363f2b6")
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
      -- ------------------------------------------------------------------------
      -- POST /branches/{branchId}/migrations/current
      -- ------------------------------------------------------------------------
      describe "POST /branches/{branchId}/migrations/current" $
        -- GIVEN: Prepare request
       do
        let branchUuid = "6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqMethod = methodPost
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
        let reqBody = encode reqDto
        it "HTTP 201 CREATED" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                MigratorStateDTO
                { _migratorStateDTOBranchUuid = fromJust . U.fromString $ branchUuid
                , _migratorStateDTOMigrationState =
                    ConflictState . CorrectorConflict . Prelude.head $ netherlandsPackageV2 ^. events
                , _migratorStateDTOBranchParentId = netherlandsPackage ^. pId
                , _migratorStateDTOTargetPackageId = netherlandsPackageV2 ^. pId
                , _migratorStateDTOCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1Netherlands
                }
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createInvalidJsonTest
          reqMethod
          reqUrl
          [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |]
          "targetPackageId"
        it "HTTP 400 BAD REQUEST when migration is already created" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "Migration is already created"
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when target parent package doesn’t exist" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "Target parent package doesn’t exist"
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          runInContextIO (deletePackageById (netherlandsPackageV2 ^. pId)) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when target Package is not higher than current one" $
          -- GIVEN: Prepare request
         do
          let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = "dsw.nl:core-nl:0.9.0"}
          let reqBody = encode reqDto
          -- AND: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_KMMT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage (netherlandsPackageV2 & version .~ "dsw.nl:core-nl:0.9.0")) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when branch has to have a parent" $
          -- AND: Prepare expectation
         do
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "Branch has to have a parent"
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          let branchUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
          let branch =
                BranchCreateDTO
                { _branchCreateDTOName = amsterdamPackage ^. name
                , _branchCreateDTOKmId = amsterdamPackage ^. kmId
                , _branchCreateDTOParentPackageId = Nothing
                }
          runInContextIO
            (createBranchWithParams branchUuid timestamp (fromJust $ appContext ^. currentUser) branch)
            appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
      -- ------------------------------------------------------------------------
      -- DELETE /users/{userId}
      -- ------------------------------------------------------------------------
      describe "DELETE /users/{userId}" $
        -- GIVEN: Prepare request
       do
        let branchUuid = "6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqMethod = methodDelete
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $
           -- GIVEN: Prepare expectation
         do
          let expStatus = 204
          let expHeaders = resCorsHeaders
           -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
           -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
           -- THEN: Find a result
          eitherMS <- runInContextIO (getCurrentMigration branchUuid) appContext
           -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
           -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherMS) `shouldBe` False
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
        createNotFoundTest reqMethod reqUrl reqHeaders reqBody "kmMigration" "6474b24b-262b-42b1-9451-008e8363f2b6"
       ------------------------------------------------------------------------
       -- POST /branches/{branchId}/migrations/current/conflict
       ------------------------------------------------------------------------
      describe "POST /branches/{branchId}/migrations/current/conflict" $
        -- GIVEN: Prepare request
       do
        let branchUuid = "6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqMethod = methodPost
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current/conflict"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto =
              MigratorConflictDTO
              { _migratorConflictDTOOriginalEventUuid = a_km1_ch4 ^. uuid
              , _migratorConflictDTOAction = MCAEdited
              , _migratorConflictDTOEvent = Just . toDTOFn . Prelude.head $ netherlandsPackageV2 ^. events
              }
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          let expBody = ""
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherMigration <- runInContextIO (getCurrentMigration "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          liftIO $ (isRight eitherMigration) `shouldBe` True
          let (Right migrationFromDb) = eitherMigration
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: DB is in right state
          liftIO $ (migrationFromDb ^. migrationState) `shouldBe` CompletedState
        createInvalidJsonTest
          reqMethod
          reqUrl
          [HJ.json| { originalEventUuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |]
          "action"
        it "HTTP 400 BAD REQUEST when originalEventUuid doesn't match with current target event" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_KMMT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT
          let expBody = encode expDto
          let reqDtoEdited =
                reqDto & originalEventUuid .~ (fromJust . U.fromString $ "30ac5193-5685-41b1-86d7-ab0b356c516a")
          let reqBodyEdited = encode reqDtoEdited
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBodyEdited
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when edit migration action has to provide target event" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_KMMT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
          let expBody = encode expDto
          let reqDtoEdited = reqDto & event .~ Nothing
          let reqBodyEdited = encode reqDtoEdited
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBodyEdited
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when you can't solve conflicts because Migration state isn't in conflict state" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_KMMT_MIGRATOR__NO_CONFLICTS_TO_SOLVE
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage netherlandsPackageV2) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          runInContextIO (solveConflictAndMigrate branchUuid reqDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
