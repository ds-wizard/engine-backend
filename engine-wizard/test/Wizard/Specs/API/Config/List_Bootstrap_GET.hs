module Wizard.Specs.API.Config.List_Bootstrap_GET
  ( list_bootstrap_GET
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
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.ClientConfigMapper

import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /configs/bootstrap
-- ------------------------------------------------------------------------
list_bootstrap_GET :: AppContext -> SpecWith Application
list_bootstrap_GET appContext =
  describe "GET /configs/bootstrap" $ do
    test_200 appContext
    test_200_with_query appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/configs/bootstrap"

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
    let expHeaders = resCtHeaderJavascript : resCorsHeaders
    let expDto = toClientConfigDTO (appContext ^. applicationConfig) defaultAppConfig
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
     -- GIVEN: Prepare request
   do
    let reqUrl = "/configs/bootstrap?callback=myFunction"
     -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderJavascript : resCorsHeaders
    let expDto = toClientConfigDTO (appContext ^. applicationConfig) defaultAppConfig
    let expBody = encodeUtf8 $ LT.concat ["myFunction(", decodeUtf8 . encode $ expDto, ")"]
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
