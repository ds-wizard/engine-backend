module Specs.API.OrganizationAPISpec where

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

import Api.Resources.Organization.OrganizationDTO
import Common.Error
import Database.DAO.Organization.OrganizationDAO
import Model.Organization.Organization
import Service.Organization.OrganizationService

import Specs.API.Common

organizationAPI context dspConfig =
  with (startWebApp context dspConfig) $ do
    describe "ORGANIZATION API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /organizations/current
      -- ------------------------------------------------------------------------
     do
      describe "GET /organizations/current" $ do
        let reqMethod = methodGet
        let reqUrl = "/organizations/current"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                OrganizationDTO
                { _orgdtoUuid =
                    fromJust . U.fromString $
                    "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
                , _orgdtoName = "Elixir Amsterdam"
                , _orgdtoGroupId = "elixir.nl.amsterdam"
                }
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
      -- PUT /organizations/current
      -- ------------------------------------------------------------------------
      describe "PUT /organizations/current" $ do
        let reqMethod = methodPut
        let reqUrl = "/organizations/current"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                OrganizationDTO
                { _orgdtoUuid =
                    fromJust . U.fromString $
                    "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
                , _orgdtoName = "EDITED: Elixir Netherlands"
                , _orgdtoGroupId = "elixir.nl.amsterdam.edited"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherOrganization <- liftIO $ getOrganization context
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherOrganization) `shouldBe` True
          let (Right organizationFromDb) = eitherOrganization
          liftIO $
            (organizationFromDb ^. orgdtoUuid) `shouldBe` (reqDto ^. orgdtoUuid)
          liftIO $
            (organizationFromDb ^. orgdtoName) `shouldBe` (reqDto ^. orgdtoName)
          liftIO $
            (organizationFromDb ^. orgdtoGroupId) `shouldBe`
            (reqDto ^. orgdtoGroupId)
        createInvalidJsonTest
          reqMethod
          reqUrl
          [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |]
          "name"
        it "HTTP 400 BAD REQUEST when groupId is not in valid format" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                OrganizationDTO
                { _orgdtoUuid =
                    fromJust . U.fromString $
                    "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
                , _orgdtoName = "EDITED: Elixir Netherlands"
                , _orgdtoGroupId = "elixir-nl"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                createErrorWithFieldError
                  ("groupId", "GroupId is not in valid format")
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
        createNoPermissionTest dspConfig reqMethod reqUrl [] "" "ORG_PERM"
