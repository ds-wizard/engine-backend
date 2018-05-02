module Specs.API.EventAPISpec where

import Control.Lens
import Control.Monad.Logger (runNoLoggingT)
import Crypto.PasswordStore
import Data.Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.ByteString.Char8 as BS
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

import Api.Resource.Event.EventDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Common.Error
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.User.UserDAO
import qualified Database.Migration.Branch.BranchMigration as KMC
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import qualified Database.Migration.Package.PackageMigration as PKG
import LensesConfig
import Model.Branch.Branch
import Model.Event.Event
import Model.User.User
import Service.Event.EventMapper
import Service.Event.EventService
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Migrator.Applicator

import Specs.API.Common
import Specs.Common

eventAPI appContext = do
  let events =
        [ AddQuestionEvent' a_km1_ch1_q1
        , AddQuestionEvent' a_km1_ch1_q2
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1
        , AddAnswerEvent' a_km1_ch1_q2_aNoFu1
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
        , AddExpertEvent' a_km1_ch1_q2_eDarth
        , AddExpertEvent' a_km1_ch1_q2_eLuke
        , AddReferenceEvent' a_km1_ch1_q2_rCh1
        , AddReferenceEvent' a_km1_ch1_q2_rCh2
        , AddChapterEvent' a_km1_ch2
        , AddQuestionEvent' a_km1_ch2_q3
        , AddAnswerEvent' a_km1_ch2_q3_aNo2
        , AddAnswerEvent' a_km1_ch2_q3_aYes2
        ]
  with (startWebApp appContext) $ do
    let context = appContext ^. oldContext
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
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ KMC.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expBody = encode . toDTOs $ events
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
        let reqBody = encode . toDTOs $ events
        it "HTTP 201 CREATED" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ KMC.runMigration appContext
          liftIO $ deleteEvents context "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expKm = km1
          let expBody = encode . toDTOs $ events
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherBranch <- liftIO $ findBranchWithEventsById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          eitherKm <- liftIO $ findBranchWithKMByBranchId context "6474b24b-262b-42b1-9451-008e8363f2b6"
          let expBody = reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. bweEvents) `shouldBe` events
          liftIO $ (isRight eitherKm) `shouldBe` True
          let (Right kmFromDb) = eitherKm
          liftIO $ (kmFromDb ^. bwkmKM) `shouldBe` (Just expKm)
        createInvalidJsonArrayTest
          reqMethod
          reqUrl
          [HJ.json| [{ uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" }] |]
          "eventType"
        it "HTTP 400 BAD REQUEST if unsupported event type" $ do
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ KMC.runMigration appContext
          liftIO $ deleteEvents context "6474b24b-262b-42b1-9451-008e8363f2b6"
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
          eitherBranch <- liftIO $ findBranchWithEventsById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          eitherKm <- liftIO $ findBranchWithKMByBranchId context "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Events and KM was not saved
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. bweEvents) `shouldBe` []
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
          liftIO . runNoLoggingT $ PKG.runMigration appContext
          liftIO . runNoLoggingT $ KMC.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          let (Right expectedKm) = runApplicator Nothing [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- THEN: Find a result
          eitherBranch <- liftIO $ findBranchWithEventsById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          eitherKm <- liftIO $ findBranchWithKMByBranchId context "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals reqBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherBranch) `shouldBe` True
          let (Right branchFromDb) = eitherBranch
          liftIO $ (branchFromDb ^. bweEvents) `shouldBe` []
          liftIO $ (isRight eitherKm) `shouldBe` True
          let (Right kmFromDb) = eitherKm
          liftIO $ (kmFromDb ^. bwkmKM) `shouldBe` (Just expectedKm)
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/events" reqHeaders reqBody
