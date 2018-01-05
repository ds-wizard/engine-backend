module Specs.API.InfoAPISpec where

import Data.Aeson (Value(..), (.=), object)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher
import qualified Web.Scotty as S

import Specs.API.Common
import Specs.Common

infoAPI context dspConfig =
  with (startWebApp context dspConfig) $ do
    describe "INFO API Spec" $
      describe "GET /" $ do
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqMethod = methodGet
          let reqUrl = "/"
          let reqHeaders = []
          let reqBody = ""
          -- GIVEN: Prepare expectation
          let expBody =
                [json|
          {
            name: "Data Stewardship Portal Server",
            version: "1.0.0",
            builtAt: "2017/10/25 19:50:20Z"
          }
          |]
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expStatus = 200
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
