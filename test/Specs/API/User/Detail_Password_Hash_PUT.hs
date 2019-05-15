module Specs.API.User.Detail_Password_Hash_PUT
  ( detail_password_hash_put
  ) where

import Control.Lens ((^.))
import Crypto.PasswordStore
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Data.Maybe
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.User.UserPasswordDTO
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.User.UserDAO
import LensesConfig
import Localization
import Model.ActionKey.ActionKey
import Model.Context.AppContext
import Model.Error.ErrorHelpers

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /users/{userId}/password?hash={hash}
-- ------------------------------------------------------------------------
detail_password_hash_put :: AppContext -> SpecWith Application
detail_password_hash_put appContext =
  describe "PUT /users/{userId}/password?hash={hash}" $ do
    test_204 appContext
    test_400_invalid_json appContext
    test_400_hash_is_not_provided appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = UserPasswordDTO {_userPasswordDTOPassword = "newPassword"}

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
  -- GIVEN: Prepare DB
   do
    let actionKey =
          ActionKey
          { _actionKeyUuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
          , _actionKeyUserId = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
          , _actionKeyAType = ForgottenPasswordActionKey
          , _actionKeyHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
          , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
          }
    eitherActionKey <- runInContextIO (insertActionKey actionKey) appContext
  -- AND: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
  -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
  -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
  -- AND: Find result in DB and compare with expectation state
    eitherUser <- runInContextIO (findUserById "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66") appContext
    liftIO $ (isRight eitherUser) `shouldBe` True
    let (Right userFromDb) = eitherUser
    let isSame = verifyPassword (BS.pack (reqDto ^. password)) (BS.pack (userFromDb ^. passwordHash))
    liftIO $ isSame `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_hash_is_not_provided appContext =
  it "HTTP 400 BAD REQUEST when hash is not provided" $
  -- GIVEN: Prepare url
   do
    let reqUrlWithoutHash = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password"
  -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto = createErrorWithErrorMessage _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS
    let expBody = encode expDto
  -- WHEN: Call API
    response <- request reqMethod reqUrlWithoutHash reqHeaders reqBody
  -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  -- AND: Find result in DB and compare with expectation state
    eitherUser <- runInContextIO (findUserById "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66") appContext
    liftIO $ (isRight eitherUser) `shouldBe` True
    let (Right userFromDb) = eitherUser
    let isSame = verifyPassword (BS.pack (reqDto ^. password)) (BS.pack (userFromDb ^. passwordHash))
    liftIO $ isSame `shouldBe` False

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=e565b3da-70bf-4991-ad47-4209514b3a67"
    reqHeaders
    reqBody
    "actionKey"
    "e565b3da-70bf-4991-ad47-4209514b3a67"
