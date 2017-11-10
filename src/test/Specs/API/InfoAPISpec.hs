module Specs.API.InfoAPISpec where

import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Web.Scotty as S

import Specs.API.Common

infoAPI context dspConfig =
  with (startWebApp context dspConfig) $ do
    describe "INFO API Spec" $
      describe "GET /" $ do
        it "HTTP 200 OK" $ do
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
          get "/" `shouldRespondWith`
            expBody {matchHeaders = expHeaders, matchStatus = expStatus}
