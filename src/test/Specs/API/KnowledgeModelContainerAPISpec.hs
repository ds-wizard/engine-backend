module Specs.API.KnowledgeModelContainerAPISpec where

import Control.Lens
import Data.Aeson
import Data.Aeson (Value(..), object, (.=))
import Data.ByteString.Lazy
import Data.Either
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
import Common.Error
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

import Specs.API.Common

kmcAPI context dspConfig = do
  with (startWebApp context dspConfig) $ do
    describe "KNOWLEDGE MODEL CONTAINER API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /kmcs
      -- ------------------------------------------------------------------------
     do
      describe "GET /kmcs" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/kmcs"
        it "HTTP 200 OK" $ do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
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
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
      -- ------------------------------------------------------------------------
      -- POST /kmcs
      -- ------------------------------------------------------------------------
      describe "POST /kmcs" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodPost
        let reqUrl = "/kmcs"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        it "HTTP 201 CREATED" $ do
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
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
          eitherKmc <-
            liftIO $
            findKnowledgeModelContainerById
              context
              "6474b24b-262b-42b1-9451-008e8363f2b6"
          liftIO $ (isRight eitherKmc) `shouldBe` True
          let (Right kmcFromDb) = eitherKmc
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createInvalidJsonTest
          reqMethod
          reqUrl
          [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |]
          "name"
        it "HTTP 400 BAD REQUEST when artifactId is not in valid format" $ do
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam.km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context reqDto
          let reqBody = encode (reqDto & kmcdtoArtifactId .~ "amsterdam.km")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                createErrorWithFieldError
                  ("artifactId", "ArtifactId is not in valid format")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when artifactId is already taken" $ do
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqBody = encode reqDto
          liftIO $ createKnowledgeModelContainer context reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                createErrorWithFieldError
                  ("artifactId", "ArtifactId is already taken")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
      -- ------------------------------------------------------------------------
      -- GET /kmcs/{kmcId}
      -- ------------------------------------------------------------------------
      describe "GET /kmcs/{kmcId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context expDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest
          reqMethod
          "/kmcs/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
          reqHeaders
          reqBody
      -- ------------------------------------------------------------------------
      -- PUT /kmcs/{kmcId}
      -- ------------------------------------------------------------------------
      describe "PUT /kmcs/{kmcId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodPut
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto =
              KnowledgeModelContainerDTO
              { _kmcdtoKmContainerUuid =
                  (fromJust
                     (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
              , _kmcdtoName = "EDITED: Amsterdam KM"
              , _kmcdtoArtifactId = "amsterdam-km"
              , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
              }
        let reqBody = encode reqDto
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          liftIO $ createKnowledgeModelContainer context expDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherKmc <-
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
          liftIO $ (isRight eitherKmc) `shouldBe` True
          let (Right kmcFromDb) = eitherKmc
          liftIO $
            (kmcFromDb ^. kmcKmcUuid) `shouldBe`
            (reqDto ^. kmcdtoKmContainerUuid)
          liftIO $ (kmcFromDb ^. kmcName) `shouldBe` (reqDto ^. kmcdtoName)
          liftIO $
            (kmcFromDb ^. kmcArtifactId) `shouldBe` (reqDto ^. kmcdtoArtifactId)
          liftIO $
            (kmcFromDb ^. kmcParentPackageId) `shouldBe`
            (reqDto ^. kmcdtoParentPackageId)
        createInvalidJsonTest
          reqMethod
          reqUrl
          [HJ.json| { uuid: "6474b24b-262b-42b1-9451-008e8363f2b6" } |]
          "name"
        it "HTTP 400 BAD REQUEST when artifactId is not in valid format" $ do
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context reqDto
          let reqBody = encode (reqDto & kmcdtoArtifactId .~ "amsterdam.km")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                createErrorWithFieldError
                  ("artifactId", "ArtifactId is not in valid format")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        it "HTTP 400 BAD REQUEST when artifactId is already taken" $ do
          let reqDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          let reqDto2 =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "a0cb5aec-5977-44fc-bd87-8cc1ddf5de6a"))
                , _kmcdtoName = "Amsterdam KM 2"
                , _kmcdtoArtifactId = "amsterdam-km-2"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context reqDto
          liftIO $ createKnowledgeModelContainer context reqDto2
          let reqBody = encode (reqDto & kmcdtoArtifactId .~ "amsterdam-km-2")
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                createErrorWithFieldError
                  ("artifactId", "ArtifactId is already taken")
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest
          reqMethod
          "/kmcs/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
          reqHeaders
          reqBody
      -- ------------------------------------------------------------------------
      -- DELETE /kmcs/{kmcId}
      -- ------------------------------------------------------------------------
      describe "DELETE /kmcs/{kmcId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodDelete
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- GIVEN: Save KMC to DB
          let kmcDto =
                KnowledgeModelContainerDTO
                { _kmcdtoKmContainerUuid =
                    (fromJust
                       (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
                , _kmcdtoName = "Amsterdam KM"
                , _kmcdtoArtifactId = "amsterdam-km"
                , _kmcdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
                }
          liftIO $ createKnowledgeModelContainer context kmcDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherKmc <-
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
          liftIO $ (isRight eitherKmc) `shouldBe` False
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "KM_PERM"
        createNotFoundTest
          reqMethod
          "/kmcs/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
          reqHeaders
          reqBody
