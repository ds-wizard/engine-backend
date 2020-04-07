module Wizard.Specs.API.User.List_Current_Password_PUT
  ( list_current_password_PUT
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Wizard.Api.Resource.User.UserDTO ()
import Wizard.Api.Resource.User.UserProfileChangeJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common

-- ------------------------------------------------------------------------
-- PUT /users/current/password
-- ------------------------------------------------------------------------
list_current_password_PUT :: AppContext -> SpecWith Application
list_current_password_PUT appContext =
  describe "PUT /users/current/password" $ do
    test_204 appContext
    test_400_invalid_json appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/current/password"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userPassword

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertPasswordOfUserInDB appContext userAlbert (userPassword ^. password)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = do
  createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
