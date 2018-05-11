module Specs.API.InfoAPISpec where

import Control.Lens ((^.))
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher

import LensesConfig

import Specs.API.Common

infoAPI appContext =
  with (startWebApp appContext) $ do
    let context = appContext ^. oldContext
    let dswConfig = appContext ^. config
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
            name: "Data Stewardship Wizard Server",
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
