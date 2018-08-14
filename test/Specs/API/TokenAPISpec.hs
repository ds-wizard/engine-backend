module Specs.API.TokenAPISpec where

import Control.Lens ((^.))
import Data.Aeson
import Network.HTTP.Types
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Token.TokenCreateDTO
import Api.Resource.Token.TokenDTO
import LensesConfig
import Model.Error.ErrorHelpers

import Specs.API.Common

tokenAPI appContext =
  with (startWebApp appContext) $
  describe "TOKEN API Spec" $
      -- ------------------------------------------------------------------------
      -- POST /tokens
      -- ------------------------------------------------------------------------
  describe "POST /tokens" $ do
    let reqMethod = methodPost
    let reqUrl = "/tokens"
    it "HTTP 201 OK" $
          -- GIVEN: Prepare request
     do
      let reqHeaders = [reqAuthHeader, reqCtHeader]
      let reqDto =
            TokenCreateDTO {_tokenCreateDTOEmail = "albert.einstein@example.com", _tokenCreateDTOPassword = "password"}
      let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
      let (SResponse (Status status _) headers body) = response
      liftIO $ status `shouldBe` expStatus
      liftIO $ headers `shouldBe` expHeaders
      let eBody = eitherDecode body :: Either String TokenDTO
      let (Right body) = eBody
      liftIO $ (body ^. token) `shouldStartWith` "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
    createInvalidJsonTest reqMethod reqUrl [HJ.json| { email: "albert.einstein@example.com" } |] "password"
    it "HTTP 400 BAD REQUEST when email or password are not valid" $
          -- GIVEN: Prepare request
     do
      let reqHeaders = [reqAuthHeader, reqCtHeader]
      let reqDto =
            TokenCreateDTO {_tokenCreateDTOEmail = "albert.einstein@example.com2", _tokenCreateDTOPassword = "password"}
      let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
      let expStatus = 400
      let expHeaders = [resCtHeader] ++ resCorsHeaders
      let expDto = createErrorWithErrorMessage "Incorrect email or password"
      let expBody = encode expDto
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
