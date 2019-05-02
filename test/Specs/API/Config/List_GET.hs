module Specs.API.Config.List_GET
  ( list_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Model.Context.AppContext
import Service.Config.ClientConfigMapper

import Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /configuration
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext =
  describe "GET /configuration" $ do
    test_200 appContext
    test_200_with_query appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/configuration"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeaderJavascript] ++ resCorsHeaders
    let expDto = toDTO (appContext ^. appConfig)
    let expBody = encodeUtf8 $ LT.concat ["callback", "(", decodeUtf8 . encode $ expDto, ")"]
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_with_query appContext =
  it "HTTP 200 OK (with query)" $
     -- GIVEN: Prepare expectation
   do
    let reqUrl = "/configuration?callback=myFunction"
    let expStatus = 200
    let expHeaders = [resCtHeaderJavascript] ++ resCorsHeaders
    let expDto = toDTO (appContext ^. appConfig)
    let expBody = encodeUtf8 $ LT.concat ["myFunction(", decodeUtf8 . encode $ expDto, ")"]
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
