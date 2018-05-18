module Specs.API.TokenAPISpec where

import Data.Aeson
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Token.TokenCreateDTO
import Api.Resource.Token.TokenDTO
import Common.Error

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
      let reqDto = TokenCreateDTO {_tcdtoEmail = "albert.einstein@example.com", _tcdtoPassword = "password"}
      let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = [resCtHeader] ++ resCorsHeaders
      let expDto =
            TokenDTO
            { _tdtoToken =
                "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInBlcm1pc3Npb25zIjpbIlVNX1BFUk0iLCJPUkdfUEVSTSIsIktNX1BFUk0iLCJLTV9VUEdSQURFX1BFUk0iLCJLTV9QVUJMSVNIX1BFUk0iLCJQTV9QRVJNIiwiUVROX1BFUk0iLCJETVBfUEVSTSJdfQ.j53sR7cehH2ccE4etKuagBloCPCXXqS8iysE1ClmSEk"
            }
      let expBody = encode expDto
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
    createInvalidJsonTest reqMethod reqUrl [HJ.json| { email: "albert.einstein@example.com" } |] "password"
    it "HTTP 400 BAD REQUEST when email or password are not valid" $
          -- GIVEN: Prepare request
     do
      let reqHeaders = [reqAuthHeader, reqCtHeader]
      let reqDto = TokenCreateDTO {_tcdtoEmail = "albert.einstein@example.com2", _tcdtoPassword = "password"}
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
