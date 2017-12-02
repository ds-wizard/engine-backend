module Specs.API.MigratorAPISpec where

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
import Api.Resources.Migrator.MigratorConflictDTO
import Api.Resources.Migrator.MigratorStateCreateDTO
import Api.Resources.Migrator.MigratorStateDTO
import Common.Error
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import qualified Database.Migration.Branch.BranchMigration as B
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Package.Data.Package
import qualified Database.Migration.Package.PackageMigration as PKG
import Model.Branch.Branch
import Model.Branch.BranchState
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Event
import Model.Migrator.MigratorState
import Model.Package.Package
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Migrator.MigratorService

import Fixtures.KnowledgeModel.KnowledgeModels
import Specs.API.Common
import Specs.Common

migratorAPI context dspConfig = do
  with (startWebApp context dspConfig) $ do
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
                { _msdtoBranchUuid = fromJust . U.fromString $ branchUuid
                , _msdtoMigrationState =
                    ConflictState . CorrectorConflict . Prelude.head $ elixirNlPackage2Dto ^. pkgweEvents
                , _msdtoBranchParentId = "elixir.nl:core-nl:1.0.0"
                , _msdtoTargetPackageId = "elixir.nl:core-nl:2.0.0"
                , _msdtoCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1
                }
          let expBody = encode expDto
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] reqBody "KM_UPGRADE_PERM"
        it "HTTP 404 NOT FOUND - migration doesn't exist" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 404
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = NotExistsError "Entity does not exist"
          let expBody = encode expDto
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
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
        let reqDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
        let reqBody = encode reqDto
        it "HTTP 201 CREATED" $ do
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                MigratorStateDTO
                { _msdtoBranchUuid = fromJust . U.fromString $ branchUuid
                , _msdtoMigrationState =
                    ConflictState . CorrectorConflict . Prelude.head $ elixirNlPackage2Dto ^. pkgweEvents
                , _msdtoBranchParentId = "elixir.nl:core-nl:1.0.0"
                , _msdtoTargetPackageId = "elixir.nl:core-nl:2.0.0"
                , _msdtoCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1
                }
          let expBody = encode expDto
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
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
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
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
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
          liftIO $ deletePackageById context (elixirNlPackage2Dto ^. pkgweId)
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when target Package is not higher than current one" $
          -- GIVEN: Prepare request
         do
          let reqDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = "elixir.nl:core-nl:0.9.0"}
          let reqBody = encode reqDto
          -- AND: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "Target Package is not higher than current one"
          let expBody = encode expDto
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context (elixirNlPackage2Dto & pkgweVersion .~ "elixir.nl:core-nl:0.9.0")
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
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
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          let branch =
                BranchDTO
                { _bdtoUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
                , _bdtoName = "Amsterdam KM"
                , _bdtoGroupId = "elixir.nl.amsterdam"
                , _bdtoArtifactId = "amsterdam-km"
                , _bdtoParentPackageId = Nothing
                , _bdtoLastAppliedParentPackageId = Nothing
                }
          liftIO $ createBranch context branch
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteMigratorStates context
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
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
        let reqDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $
           -- GIVEN: Prepare expectation
         do
          let expStatus = 204
          let expHeaders = resCorsHeaders
           -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
           -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
           -- THEN: Find a result
          eitherMS <- liftIO $ findMigratorStateByBranchUuid context branchUuid
           -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
           -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherMS) `shouldBe` False
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
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
              { _mcdtoOriginalEventUuid = a_km1_ch2 ^. achUuid
              , _mcdtoAction = MCAEdited
              , _mcdtoEvent = Just . toDTOFn . Prelude.head $ elixirNlPackage2Dto ^. pkgweEvents
              }
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $ do
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          let expBody = ""
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherMigration <- liftIO $ getCurrentMigration context "6474b24b-262b-42b1-9451-008e8363f2b6"
          liftIO $ (isRight eitherMigration) `shouldBe` True
          let (Right migrationFromDb) = eitherMigration
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: DB is in right state
          liftIO $ (migrationFromDb ^. msdtoMigrationState) `shouldBe` CompletedState
        createInvalidJsonTest
          reqMethod
          reqUrl
          [HJ.json| { originalEventUuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |]
          "action"
        it "HTTP 400 BAD REQUEST when originalEventUuid doesn't match with current target event" $ do
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "OriginalEventUuid doesn't match with current target event"
          let expBody = encode expDto
          let reqDtoEdited =
                reqDto & mcdtoOriginalEventUuid .~ (fromJust . U.fromString $ "30ac5193-5685-41b1-86d7-ab0b356c516a")
          let reqBodyEdited = encode reqDtoEdited
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBodyEdited
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when edit action has to provide target event" $ do
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "Edit action has to provide target event"
          let expBody = encode expDto
          let reqDtoEdited = reqDto & mcdtoEvent .~ Nothing
          let reqBodyEdited = encode reqDtoEdited
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBodyEdited
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when you can't solve conflicts because Migration state isn't in conflict state" $ do
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = MigratorError "You can't solve conflicts because Migration state isn't in conflict state"
          let expBody = encode expDto
          -- AND: Prepare database
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ B.runMigration context dspConfig fakeLogState
          liftIO $ insertPackage context elixirNlPackage2Dto
          liftIO $ deleteEventsAtBranch context branchUuid
          liftIO $ deleteMigratorStates context
          let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
          liftIO $ createMigration context branchUuid migratorCreateDto
          liftIO $ solveConflictAndMigrate context branchUuid reqDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_UPGRADE_PERM"
