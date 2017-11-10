module Specs.API.KnowledgeModelContainerAPISpec where

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

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

import Specs.API.Common

--shouldRespondWith :: HasCallStack => WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith r matcher = do
  forM_ (match r matcher) (liftIO . expectationFailure)

kmcAPI context dspConfig = do
  with (startWebApp context dspConfig) $ do
    describe "KNOWLEDGE MODEL CONTAINER API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /kmcs
      -- ------------------------------------------------------------------------
     do
      describe "GET /kmcs" $ do
        let reqMethod = methodGet
        let reqUrl = "/kmcs"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "KM Container from Amsterdam"
                , _kmcdtoShortName = "elixir-nl-ams"
                , _kmcdtoParentPackageName = "elixir-nl"
                , _kmcdtoParentPackageVersion = "1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context expDto
          let expBody = encode [expDto]
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
      -- POST /kmcs
      -- ------------------------------------------------------------------------
      describe "POST /kmcs" $ do
        let reqMethod = methodPost
        let reqUrl = "/kmcs"
        it "HTTP 201 CREATED" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "KM Container from Amsterdam"
                , _kmcdtoShortName = "elixir-nl-ams"
                , _kmcdtoParentPackageName = "elixir-nl"
                , _kmcdtoParentPackageVersion = "1.0.0"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          maybeKmc <-
            liftIO $
            findKnowledgeModelContainerById
              context
              "6474b24b-262b-42b1-9451-008e8363f2b6"
          liftIO $ (isJust maybeKmc) `shouldBe` True
          let kmcFromDb = fromJust maybeKmc
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $
            (kmcFromDb ^. kmcKmContainerUuid) `shouldBe`
            (reqDto ^. kmcdtoKmContainerUuid)
          liftIO $ (kmcFromDb ^. kmcName) `shouldBe` (reqDto ^. kmcdtoName)
          liftIO $
            (kmcFromDb ^. kmcShortName) `shouldBe` (reqDto ^. kmcdtoShortName)
          liftIO $
            (kmcFromDb ^. kmcParentPackageName) `shouldBe`
            (reqDto ^. kmcdtoParentPackageName)
          liftIO $
            (kmcFromDb ^. kmcParentPackageVersion) `shouldBe`
            (reqDto ^. kmcdtoParentPackageVersion)
        createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- GET /kmcs/{kmcId}
      -- ------------------------------------------------------------------------
      describe "GET /kmcs/{kmcId}" $ do
        let reqMethod = methodGet
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "KM Container from Amsterdam"
                , _kmcdtoShortName = "elixir-nl-ams"
                , _kmcdtoParentPackageName = "elixir-nl"
                , _kmcdtoParentPackageVersion = "1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context expDto
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
      -- PUT /kmcs/{kmcId}
      -- ------------------------------------------------------------------------
      describe "PUT /kmcs/{kmcId}" $ do
        let reqMethod = methodPut
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "EDITED: KM Container from Amsterdam"
                , _kmcdtoShortName = "EDITED: elixir-nl-ams"
                , _kmcdtoParentPackageName = "EDITED: elixir-nl"
                , _kmcdtoParentPackageVersion = "9.9.9"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          liftIO $ createKnowledgeModelContainer context expDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          maybeKmc <-
            liftIO $
            findKnowledgeModelContainerById
              context
              "6474b24b-262b-42b1-9451-008e8363f2b6"
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
          liftIO $
            (kmcFromDb ^. kmcKmContainerUuid) `shouldBe`
            (reqDto ^. kmcdtoKmContainerUuid)
          liftIO $ (kmcFromDb ^. kmcName) `shouldBe` (reqDto ^. kmcdtoName)
          liftIO $
            (kmcFromDb ^. kmcShortName) `shouldBe` (reqDto ^. kmcdtoShortName)
          liftIO $
            (kmcFromDb ^. kmcParentPackageName) `shouldBe`
            (reqDto ^. kmcdtoParentPackageName)
          liftIO $
            (kmcFromDb ^. kmcParentPackageVersion) `shouldBe`
            (reqDto ^. kmcdtoParentPackageVersion)
        createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- DELETE /kmcs/{kmcId}
      -- ------------------------------------------------------------------------
      describe "DELETE /kmcs/{kmcId}" $ do
        let reqMethod = methodDelete
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6"
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- GIVEN: Save KMC to DB
          let kmcDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "KM Container from Amsterdam"
                , _kmcdtoShortName = "elixir-nl-ams"
                , _kmcdtoParentPackageName = "elixir-nl"
                , _kmcdtoParentPackageVersion = "1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context kmcDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- THEN: Find a result
          maybeKmc <-
            liftIO $
            findKnowledgeModelContainerById
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
          liftIO $ (isJust maybeKmc) `shouldBe` False
        createAuthTest reqMethod reqUrl [] ""
