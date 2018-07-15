module Specs.API.OrganizationAPISpec where

import Control.Lens
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
import Common.Localization
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Model.Error.ErrorHelpers
import Service.Organization.OrganizationService

import Specs.API.Common
import Specs.Common

organizationAPI appContext =
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. config
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
                { _organizationDTOUuid = org1 ^. uuid
                , _organizationDTOName = org1 ^. name
                , _organizationDTOOrganizationId = org1 ^. organizationId
                , _organizationDTOCreatedAt = org1 ^. createdAt
                , _organizationDTOUpdatedAt = org1 ^. updatedAt
                }
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
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
                OrganizationChangeDTO
                { _organizationChangeDTOUuid = fromJust . U.fromString $ "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
                , _organizationChangeDTOName = "EDITED: Elixir Netherlands"
                , _organizationChangeDTOOrganizationId = "elixir.nl.amsterdam.edited"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherOrganization <- runInContextIO getOrganization appContext
          -- AND: Compare response with expetation
          let (SResponse (Status status _) headers body) = response
          liftIO $ status `shouldBe` expStatus
          liftIO $ headers `shouldBe` expHeaders
          let (Right resBody) = eitherDecode body :: Either String OrganizationDTO
          liftIO $ (resBody ^. uuid) `shouldBe` (reqDto ^. uuid)
          liftIO $ (resBody ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (resBody ^. organizationId) `shouldBe` (reqDto ^. organizationId)
          -- AND: Compare state in DB with expetation
          liftIO $ (isRight eitherOrganization) `shouldBe` True
          let (Right organizationFromDb) = eitherOrganization
          liftIO $ (organizationFromDb ^. uuid) `shouldBe` (reqDto ^. uuid)
          liftIO $ (organizationFromDb ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (organizationFromDb ^. organizationId) `shouldBe` (reqDto ^. organizationId)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |] "name"
        it "HTTP 400 BAD REQUEST when organizationId is not in valid format" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                OrganizationDTO
                { _organizationDTOUuid = fromJust . U.fromString $ "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
                , _organizationDTOName = "EDITED: Elixir Netherlands"
                , _organizationDTOOrganizationId = "elixir-nl"
                , _organizationDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
                , _organizationDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("organizationId", _ERROR_VALIDATION__INVALID_GROUPID_FORMAT)
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "ORG_PERM"
