module Specs.API.EventAPISpec where

import Control.Lens
import Crypto.PasswordStore
import Data.Aeson
import Data.Aeson (Value(..), object, (.=))
import qualified Data.ByteString.Char8 as BS
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

import Api.Resources.Event.EventDTO
import Api.Resources.User.UserDTO
import Api.Resources.User.UserPasswordDTO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.User.UserDAO
import Database.Migration.KnowledgeModel.Data.Event.Event
import qualified
       Database.Migration.KnowledgeModel.KnowledgeModelContainerMigration
       as KMC
import qualified Database.Migration.Package.PackageMigration as PKG
import KMMigration.Migration.Migration
import Model.Event.Event
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.User.User
import Service.Event.EventMapper
import Service.Event.EventService
import Service.KnowledgeModel.KnowledgeModelMapper

import Fixtures.KnowledgeModel.KnowledgeModels
import Specs.API.Common

eventAPI context dspConfig = do
  let events =
        [ AddQuestionEvent' a_km1_ch1_q1
        , AddQuestionEvent' a_km1_ch1_q2
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1
        , AddAnswerEvent' a_km1_ch1_q2_aNo3
        , AddAnswerEvent' a_km1_ch1_q2_aYes3
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        , AddAnswerEvent' a_km1_ch1_q2_aNo4
        , AddAnswerEvent' a_km1_ch1_q2_aYes4
        , AddExpertEvent' a_km1_ch1_q2_eDarth
        , AddExpertEvent' a_km1_ch1_q2_eLuke
        , AddReferenceEvent' a_km1_ch1_q2_rCh1
        , AddReferenceEvent' a_km1_ch1_q2_rCh2
        , AddChapterEvent' a_km1_ch2
        , AddQuestionEvent' a_km1_ch2_q3
        , AddAnswerEvent' a_km1_ch2_q3_aNo2
        , AddAnswerEvent' a_km1_ch2_q3_aYes2
        ]
  with (startWebApp context dspConfig) $ do
    describe "EVENT API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /kmcs/{kmcId}/events
      -- ------------------------------------------------------------------------
     do
      describe "GET /kmcs/{kmcId}/events" $ do
        let reqMethod = methodGet
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/events"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expBody = encode . toDTOs $ events
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- POST /kmcs/{kmcId}/events/_bulk
      -- ------------------------------------------------------------------------
      describe "POST /kmcs/{kmcId}/events/_bulk" $ do
        let reqMethod = methodPost
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/events/_bulk"
        it "HTTP 201 CREATED" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqBody = encode . toDTOs $ events
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          liftIO $ deleteEvents context "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expKm = km1
          let expBody = encode . toDTOs $ events
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          maybeKmc <-
            liftIO $
            findKmcWithEventsById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          maybeKm <-
            liftIO $
            findKnowledgeModelByKmcId
              context
              "6474b24b-262b-42b1-9451-008e8363f2b6"
          let expBody = reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isJust maybeKmc) `shouldBe` True
          let kmcFromDb = fromJust maybeKmc
          liftIO $ (kmcFromDb ^. kmcweEvents) `shouldBe` events
          let kmFromDb = fromJust maybeKm
          liftIO $ (kmFromDb ^. kmcwkmKM) `shouldBe` expKm
      -- ------------------------------------------------------------------------
      -- DELETE /kmcs/{kmcId}/events
      -- ------------------------------------------------------------------------
      describe "DELETE /kmcs/{kmcId}/events" $ do
        let reqMethod = methodDelete
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/events"
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          let expectedKm =
                migrate
                  Nothing
                  [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- THEN: Find a result
          maybeKmc <-
            liftIO $
            findKmcWithEventsById context "6474b24b-262b-42b1-9451-008e8363f2b6"
          maybeKm <-
            liftIO $
            findKnowledgeModelByKmcId
              context
              "6474b24b-262b-42b1-9451-008e8363f2b6"
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals ""
                }
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isJust maybeKmc) `shouldBe` True
          let kmcFromDb = fromJust maybeKmc
          liftIO $ (kmcFromDb ^. kmcweEvents) `shouldBe` []
          let kmFromDb = fromJust maybeKm
          liftIO $ (kmFromDb ^. kmcwkmKM) `shouldBe` expectedKm
        createAuthTest reqMethod reqUrl [] ""
