module Specs.API.ActionKey.List_POST
  ( list_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.ActionKey.ActionKeyJM ()
import Database.DAO.ActionKey.ActionKeyDAO
import Database.Migration.Development.ActionKey.Data.ActionKeys
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Model.Context.AppContext

import Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /action-keys
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /action-keys" $ do
    test_201 appContext
    test_400_invalid_json appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/action-keys"

reqHeaders = [reqCtHeader]

reqDto = forgTokActionKeyDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    actionKeyFromDb <- getFirstFromDB findActionKeys appContext
    liftIO $ (actionKeyFromDb ^. aType) `shouldBe` reqDto ^. aType
    liftIO $ actionKeyFromDb ^. organizationId `shouldBe` orgDsw ^. organizationId

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common KM" } |] "type"
