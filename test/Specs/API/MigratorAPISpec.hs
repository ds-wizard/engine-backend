module Specs.API.MigratorAPISpec where

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
import Api.Resource.Migrator.MigratorConflictDTO
import Api.Resource.Migrator.MigratorStateCreateDTO
import Api.Resource.Migrator.MigratorStateDTO
import Common.Error
import Common.Localization
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import qualified Database.Migration.Branch.BranchMigration as B
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.Package.Data.Package
import qualified Database.Migration.Package.PackageMigration as PKG
import LensesConfig
import Model.Migrator.MigratorState
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Migrator.MigratorService

import Specs.API.Common
import Specs.Common

migratorAPI appContext = do
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. config
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
                    ConflictState . CorrectorConflict . Prelude.head $ elixirNlPackage2Dto ^. events
                , _migratorStateDTOBranchParentId = "elixir.nl:core-nl:1.0.0"
                , _migratorStateDTOTargetPackageId = "elixir.nl:core-nl:2.0.0"
                , _migratorStateDTOCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1
                }
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
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
          let expDto = NotExistsError "Entity does not exist"
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
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
        let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
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
                    ConflictState . CorrectorConflict . Prelude.head $ elixirNlPackage2Dto ^. events
                , _migratorStateDTOBranchParentId = "elixir.nl:core-nl:1.0.0"
                , _migratorStateDTOTargetPackageId = "elixir.nl:core-nl:2.0.0"
                , _migratorStateDTOCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1
                }
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
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
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
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
          runInContextIO (deletePackageById (elixirNlPackage2Dto ^. pId)) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when target Package is not higher than current one" $
          -- GIVEN: Prepare request
         do
          let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = "elixir.nl:core-nl:0.9.0"}
          let reqBody = encode reqDto
          -- AND: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_MT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage (elixirNlPackage2Dto & version .~ "elixir.nl:core-nl:0.9.0")) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
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
          let branch =
                BranchDTO
                { _branchDTOUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
                , _branchDTOName = "Amsterdam KM"
                , _branchDTOOrganizationId = "elixir.nl.amsterdam"
                , _branchDTOKmId = "amsterdam-km"
                , _branchDTOParentPackageId = Nothing
                , _branchDTOLastAppliedParentPackageId = Nothing
                }
          runInContextIO (createBranch branch) appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
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
        let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $
           -- GIVEN: Prepare expectation
         do
          let expStatus = 204
          let expHeaders = resCorsHeaders
           -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
           -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
           -- THEN: Find a result
          eitherMS <- runInContextIO (findMigratorStateByBranchUuid branchUuid) appContext
           -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
           -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherMS) `shouldBe` False
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
        createNotFoundTest reqMethod reqUrl reqHeaders reqBody
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
              { _migratorConflictDTOOriginalEventUuid = a_km1_ch3 ^. uuid
              , _migratorConflictDTOAction = MCAEdited
              , _migratorConflictDTOEvent = Just . toDTOFn . Prelude.head $ elixirNlPackage2Dto ^. events
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
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherMigration <- runInContextIO (getCurrentMigration "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          liftIO $ (isRight eitherMigration) `shouldBe` True
          let (Right migrationFromDb) = eitherMigration
          -- AND: Compare response with expetation
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
          let expDto = MigratorError _ERROR_MT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT
          let expBody = encode expDto
          let reqDtoEdited =
                reqDto & originalEventUuid .~ (fromJust . U.fromString $ "30ac5193-5685-41b1-86d7-ab0b356c516a")
          let reqBodyEdited = encode reqDtoEdited
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBodyEdited
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when edit migration action has to provide target event" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_MT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
          let expBody = encode expDto
          let reqDtoEdited = reqDto & event .~ Nothing
          let reqBodyEdited = encode reqDtoEdited
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBodyEdited
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when you can't solve conflicts because Migration state isn't in conflict state" $ do
          runInContextIO PKG.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError _ERROR_MT_MIGRATOR__NO_CONFLICTS_TO_SOLVE
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage elixirNlPackage2Dto) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = elixirNlPackage2Dto ^. pId}
          runInContextIO (createMigration branchUuid migratorCreateDto) appContext
          runInContextIO (solveConflictAndMigrate branchUuid reqDto) appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
