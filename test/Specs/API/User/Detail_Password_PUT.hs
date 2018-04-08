module Specs.API.User.Detail_Password_PUT
  ( detail_password_put
  ) where

import Control.Lens ((^.))
import Crypto.PasswordStore
import Data.Aeson (Value(..), (.=), encode, object)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy
import Data.Either
import Data.Foldable
import Data.Maybe
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher
import qualified Web.Scotty as S

import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserStateDTO
import Common.Context
import Model.Config.DSWConfig
import Common.Error
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.User.UserDAO
import Model.ActionKey.ActionKey
import Model.User.User
import Service.ActionKey.ActionKeyService
import Service.User.UserService

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /users/{userId}/password
-- ------------------------------------------------------------------------
detail_password_put :: Context -> DSWConfig -> SpecWith Application
detail_password_put context dswConfig =
  describe "PUT /users/{userId}/password" $ do
    test_204 context dswConfig
    test_400_invalid_json context dswConfig
    test_403_no_hash context dswConfig
    test_404 context dswConfig

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = UserPasswordDTO {_updtoPassword = "newPassword"}

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 context dswConfig =
  it "HTTP 204 NO CONTENT" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Find a result
    eitherUser <- liftIO $ findUserById context "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Compare state in DB with expectation
    liftIO $ (isRight eitherUser) `shouldBe` True
    let (Right userFromDb) = eitherUser
    let isSame = verifyPassword (BS.pack (reqDto ^. updtoPassword)) (BS.pack (userFromDb ^. uPasswordHash))
    liftIO $ isSame `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json context dswConfig = createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403_no_hash context dswConfig =
  it "HTTP 400 BAD REQUEST when no hash/token is provided" $
   -- GIVEN: Prepare request
   do
    let reqHeaders = [reqCtHeader]
  -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto =
          createErrorWithErrorMessage "You have to log in as Administrator or you have to provide a hash in query param"
    let expBody = encode expDto
  -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
  -- THEN: Find a result
    eitherUser <- liftIO $ findUserById context "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
  -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  -- AND: Compare state in DB with expectation
    liftIO $ (isRight eitherUser) `shouldBe` True
    let (Right userFromDb) = eitherUser
    let isSame = verifyPassword (BS.pack (reqDto ^. updtoPassword)) (BS.pack (userFromDb ^. uPasswordHash))
    liftIO $ isSame `shouldBe` False

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 context dswConfig =
  createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0/password" reqHeaders reqBody
