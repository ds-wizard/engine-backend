module Specs.API.EventAPISpec where

import Control.Lens
import Data.Aeson
import Data.Either
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import qualified
       Database.Migration.Development.Branch.BranchMigration as KMC
import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import LensesConfig
import Model.Error.ErrorHelpers
import Model.Event.Event
import Service.Event.EventMapper
import Service.Event.EventService
import Service.Migrator.Applicator.Applicator

import Specs.API.Common
import Specs.Common

eventAPI appContext = do
  let bEvents =
        [ AddQuestionEvent' a_km1_ch1_q1
        , AddQuestionEvent' a_km1_ch1_q2
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
        , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
        , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
        , AddExpertEvent' a_km1_ch1_q2_eAlbert
        , AddExpertEvent' a_km1_ch1_q2_eNikola
        , AddReferenceEvent' a_km1_ch1_q2_rCh1
        , AddReferenceEvent' a_km1_ch1_q2_rCh2
        , AddChapterEvent' a_km1_ch2
        , AddQuestionEvent' a_km1_ch2_q3
        , AddAnswerEvent' a_km1_ch2_q3_aNo2
        , AddAnswerEvent' a_km1_ch2_q3_aYes2
        ]
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. config
    describe "EVENT API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /branches/{branchId}/events
      -- ------------------------------------------------------------------------
     do
      describe "GET /branches/{branchId}/events" $ do
        let reqMethod = methodGet
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/events"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          runInContextIO PKG.runMigration appContext
          runInContextIO KMC.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expBody = encode . toDTOs $ bEvents
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/events" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- POST /branches/{branchId}/events/_bulk
      -- ------------------------------------------------------------------------
      describe "POST /branches/{branchId}/events/_bulk" $
          -- GIVEN: Prepare request
       do
        let reqMethod = methodPost
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/events/_bulk"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = encode . toDTOs $ bEvents
        it "HTTP 201 CREATED" $ do
          runInContextIO PKG.runMigration appContext
          runInContextIO KMC.runMigration appContext
          runInContextIO (deleteEvents "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expKm = km1
          let expBody = encode . toDTOs $ bEvents
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchWithEventsById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          eitherKm <- runInContextIO (findBranchWithKMByBranchId "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          let expBody = reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. events) `shouldBe` bEvents
          liftIO $ (isRight eitherKm) `shouldBe` True
          let (Right kmFromDb) = eitherKm
          liftIO $ (kmFromDb ^. knowledgeModel) `shouldBe` (Just expKm)
        createInvalidJsonArrayTest
          reqMethod
          reqUrl
          [HJ.json| [{ uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" }] |]
          "eventType"
        it "HTTP 400 BAD REQUEST if unsupported event type" $ do
          runInContextIO PKG.runMigration appContext
          runInContextIO KMC.runMigration appContext
          runInContextIO (deleteEvents "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          let reqBody =
                [HJ.json|
                    [
                      {
                        uuid: "6474b24b-262b-42b1-9451-008e8363f2b6",
                        eventType: "NonexistingEventType"
                      }
                    ]
                  |]
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithErrorMessage "Error in $[0]: One of the events has unsupported eventType"
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchWithEventsById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          eitherKm <- runInContextIO (findBranchWithKMByBranchId "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Events and KM was not saved
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. events) `shouldBe` []
          liftIO $ (isLeft eitherKm) `shouldBe` False
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/events/_bulk" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- DELETE /branches/{branchId}/events
      -- ------------------------------------------------------------------------
      describe "DELETE /branches/{branchId}/events" $ do
        let reqMethod = methodDelete
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/events"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare request
         do
          runInContextIO PKG.runMigration appContext
          runInContextIO KMC.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          let (Right expectedKm) = runApplicator Nothing [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- THEN: Find a result
          eitherBranch <- runInContextIO (findBranchWithEventsById "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          eitherKm <- runInContextIO (findBranchWithKMByBranchId "6474b24b-262b-42b1-9451-008e8363f2b6") appContext
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals reqBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. events) `shouldBe` []
          liftIO $ (isRight eitherKm) `shouldBe` True
          let (Right kmFromDb) = eitherKm
          liftIO $ (kmFromDb ^. knowledgeModel) `shouldBe` (Just expectedKm)
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/events" reqHeaders reqBody
