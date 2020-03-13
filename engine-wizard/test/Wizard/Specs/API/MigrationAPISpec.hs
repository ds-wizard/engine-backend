module Wizard.Specs.API.MigrationAPISpec where

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

import LensesConfig
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Database.DAO.Package.PackageDAO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Localization.Messages.Public
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchService
import Wizard.Service.Event.EventMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.Migration.KnowledgeModel.MigratorService

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

timestamp = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0

migratorAPI appContext = do
  with (startWebApp appContext) $ do
    let serverConfig = appContext ^. applicationConfig
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
                      ConflictStateDTO . CorrectorConflict . Prelude.head $ netherlandsPackageV2 ^. events
                  , _migratorStateDTOBranchPreviousPackageId = netherlandsPackage ^. pId
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
        createNoPermissionTest serverConfig reqMethod reqUrl [] reqBody "KM_UPGRADE_PERM"
        it "HTTP 404 NOT FOUND - migration doesn't exist" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 404
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                createNotExistsError
                  (_ERROR_DATABASE__ENTITY_NOT_FOUND "kmMigration" "6474b24b-262b-42b1-9451-008e8363f2b6")
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
                      ConflictStateDTO . CorrectorConflict . Prelude.head $ netherlandsPackageV2 ^. events
                  , _migratorStateDTOBranchPreviousPackageId = netherlandsPackage ^. pId
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
          let expDto = createUserError _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS
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
        it "HTTP 400 BAD REQUEST when target Package is not higher than current one" $
          -- GIVEN: Prepare request
         do
          let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = "org.nl:core-nl:0.9.0"}
          let reqBody = encode reqDto
          -- AND: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createUserError _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          runInContextIO (insertPackage (netherlandsPackageV2 & version .~ "org.nl:core-nl:0.9.0")) appContext
          runInContextIO (deleteEventsAtBranch branchUuid) appContext
          runInContextIO deleteMigratorStates appContext
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when branch has to have a previous package" $
          -- AND: Prepare expectation
         do
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createUserError _ERROR_VALIDATION__BRANCH_PREVIOUS_PKG_ABSENCE
          let expBody = encode expDto
          -- AND: Prepare database
          runInContextIO PKG.runMigration appContext
          let branchUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
          let branch =
                BranchCreateDTO
                  { _branchCreateDTOName = amsterdamPackage ^. name
                  , _branchCreateDTOKmId = amsterdamPackage ^. kmId
                  , _branchCreateDTOPreviousPackageId = Nothing
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
        createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
        createNoPermissionTest serverConfig reqMethod reqUrl [reqCtHeader] reqBody "KM_UPGRADE_PERM"
        it "HTTP 404 NOT FOUND when target previous package doesn’t exist" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 404
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createNotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "package" "org.nl:core-nl:2.0.0")
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
        createNoPermissionTest serverConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
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
          let expDto = createUserError _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH
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
          let expDto = createUserError _ERROR_SERVICE_MIGRATION_KM__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
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
          let expDto = createUserError _ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE
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
        createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
        createNoPermissionTest serverConfig reqMethod reqUrl [reqCtHeader] reqBody "KM_UPGRADE_PERM"
