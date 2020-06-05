module Wizard.Specs.API.UserAPISpec where

import Control.Lens
import Data.Aeson
import Data.Either
import Data.Maybe
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.User.User
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Util.List (elems)

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

userAPI appContext = do
  let serverCfg = appContext ^. serverConfig
  -- ------------------------------------------------------------------------
  -- GET /users
  -- ------------------------------------------------------------------------
  describe "GET /users" $ do
    let reqMethod = methodGet
    let reqUrl = "/users"
    it "HTTP 200 OK" $
          -- GIVEN: Prepare request
     do
      let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
      let expStatus = 200
      let expHeaders = [resCtHeader] ++ resCorsHeaders
      let expDto = [toDTO userAlbert]
      let expBody = encode expDto
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
    createAuthTest reqMethod reqUrl [] ""
    createNoPermissionTest appContext reqMethod reqUrl [] "" "UM_PERM"
  -- ------------------------------------------------------------------------
  -- POST /users
  -- ------------------------------------------------------------------------
  describe "POST /users" $ do
    let reqMethod = methodPost
    let reqUrl = "/users"
    it "HTTP 201 CREATED" $
          -- GIVEN: Clear DB
     do
      runInContextIO deleteActionKeys appContext
          -- AND: Prepare request
      let reqHeaders = [reqAuthHeader, reqCtHeader]
      let reqDto = userJohnCreate
      let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
      eitherUser <- runInContextIO (findUserByEmail (reqDto ^. email)) appContext
      liftIO $ (isRight eitherUser) `shouldBe` True
      let (Right userFromDb) = eitherUser
          -- AND: Compare response with expectation
      let (SResponse (Status status _) headers body) = response
      liftIO $ status `shouldBe` expStatus
      liftIO $ (expHeaders `elems` headers) `shouldBe` True
          -- AND: Compare state in DB with expectation
      liftIO $ (userFromDb ^. firstName) `shouldBe` (reqDto ^. firstName)
      liftIO $ (userFromDb ^. lastName) `shouldBe` (reqDto ^. lastName)
      liftIO $ (userFromDb ^. email) `shouldBe` (reqDto ^. email)
      liftIO $ (userFromDb ^. role) `shouldBe` fromJust (reqDto ^. role)
          -- THEN: Check created action Key
      eitherActionKeys <- runInContextIO findActionKeys appContext
      liftIO $ (isRight eitherActionKeys) `shouldBe` True
      let (Right actionKeys) = eitherActionKeys
      liftIO $ Prelude.length actionKeys `shouldBe` 1
      let actionKey = actionKeys !! 00
      liftIO $ (actionKey ^. userId) `shouldBe` (userFromDb ^. uuid)
      liftIO $ (actionKey ^. aType) `shouldBe` RegistrationActionKey
      liftIO $ Prelude.length (actionKey ^. hash) > 0 `shouldBe` True
    createInvalidJsonTest reqMethod reqUrl [HJ.json| { firstName: "Albert" } |] "lastName"
    it "HTTP 400 BAD REQUEST if email is already registered" $
          -- GIVEN: Prepare request
     do
      let reqHeaders = [reqAuthHeader, reqCtHeader]
      let reqDto =
            UserCreateDTO
              { _userCreateDTOFirstName = "Albert"
              , _userCreateDTOLastName = "Einstein"
              , _userCreateDTOEmail = "albert.einstein@example.com"
              , _userCreateDTOAffiliation = Nothing
              , _userCreateDTORole = Just _USER_ROLE_ADMIN
              , _userCreateDTOPassword = "password"
              }
      let reqBody = encode reqDto
           -- GIVEN: Prepare expectation
      let expStatus = 400
      let expHeaders = [resCtHeader] ++ resCorsHeaders
      let expDto = createValidationError [] [("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)]
      let expBody = encode expDto
           -- WHEN: Call APIA
      response <- request reqMethod reqUrl reqHeaders reqBody
           -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  -- ------------------------------------------------------------------------
  -- GET /users/{userId}
  -- ------------------------------------------------------------------------
  describe "GET /users/{userId}" $
      -- GIVEN: Prepare request
   do
    let reqMethod = methodGet
    let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqBody = ""
    it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
     do
      let expStatus = 200
      let expHeaders = [resCtHeader] ++ resCorsHeaders
      let expDto = toDTO userAlbert
      let expBody = encode expDto
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
    createAuthTest reqMethod reqUrl [] ""
    createNoPermissionTest appContext reqMethod reqUrl [] "" "UM_PERM"
    createNotFoundTest
      reqMethod
      "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
      reqHeaders
      reqBody
      "user"
      "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
  -- ------------------------------------------------------------------------
  -- PUT /users/{userId}
  -- ------------------------------------------------------------------------
  describe "PUT /users/{userId}" $
        -- GIVEN: Prepare request
   do
    let reqMethod = methodPut
    let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqDto = userIsaacChange
    let reqBody = encode reqDto
    it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
     do
      let expStatus = 200
      let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
      eitherUser <- runInContextIO (findUserByEmail (reqDto ^. email)) appContext
          -- AND: Compare response with expectation
      let (SResponse (Status status _) headers body) = response
      liftIO $ status `shouldBe` expStatus
      liftIO $ (expHeaders `elems` headers) `shouldBe` True
      let (Right resBody) = eitherDecode body :: Either String UserDTO
      liftIO $ (resBody ^. firstName) `shouldBe` (reqDto ^. firstName)
      liftIO $ (resBody ^. lastName) `shouldBe` (reqDto ^. lastName)
      liftIO $ (resBody ^. email) `shouldBe` (reqDto ^. email)
          -- AND: Compare state in DB with expectation
      liftIO $ (isRight eitherUser) `shouldBe` True
      let (Right userFromDb) = eitherUser
      liftIO $ (userFromDb ^. uuid) `shouldBe` (reqDto ^. uuid)
      liftIO $ (userFromDb ^. firstName) `shouldBe` (reqDto ^. firstName)
      liftIO $ (userFromDb ^. lastName) `shouldBe` (reqDto ^. lastName)
      liftIO $ (userFromDb ^. email) `shouldBe` (reqDto ^. email)
      liftIO $ (userFromDb ^. role) `shouldBe` (reqDto ^. role)
    createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |] "firstName"
    it "HTTP 400 BAD REQUEST if email is already registered" $
         -- GIVEN: Prepare request
     do
      let reqHeaders = [reqAuthHeader, reqCtHeader]
      let johnUuid = fromJust . U.fromString $ "cb877c12-2654-41ae-a7b3-6f444d57af7f"
      let johnDto =
            UserCreateDTO
              { _userCreateDTOFirstName = "John"
              , _userCreateDTOLastName = "Doe"
              , _userCreateDTOEmail = "john.doe@example.com"
              , _userCreateDTOAffiliation = Nothing
              , _userCreateDTORole = Just _USER_ROLE_ADMIN
              , _userCreateDTOPassword = "password"
              }
      runInContextIO (createUserByAdminWithUuid johnDto johnUuid) appContext
      let reqDto =
            UserChangeDTO
              { _userChangeDTOUuid = johnUuid
              , _userChangeDTOFirstName = "EDITED: Isaac"
              , _userChangeDTOLastName = "EDITED: Newton"
              , _userChangeDTOEmail = "albert.einstein@example.com"
              , _userChangeDTOAffiliation = Nothing
              , _userChangeDTORole = _USER_ROLE_ADMIN
              , _userChangeDTOActive = True
              }
      let reqBody = encode reqDto
           -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = [resCtHeader] ++ resCorsHeaders
      let expDto = createValidationError [] [("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)]
      let expBody = encode expDto
           -- WHEN: Call APIA
      response <- request reqMethod "/users/cb877c12-2654-41ae-a7b3-6f444d57af7f" reqHeaders reqBody
           -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
    createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
    createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "UM_PERM"
    createNotFoundTest
      reqMethod
      "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
      reqHeaders
      reqBody
      "user"
      "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
  -- ------------------------------------------------------------------------
  -- PUT /users/{userId}/state?hash={hash}
  -- ------------------------------------------------------------------------
  describe "PUT /users/{userId}/state?hash={hash}" $
        -- GIVEN:Prepare request
   do
    let reqMethod = methodPut
    let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    let reqHeaders = [reqCtHeader]
    let reqDto = userState
    let reqBody = encode reqDto
    it "HTTP 204 NO CONTENT" $
          -- AND: Prepare DB
     do
      eitherActionKey <- runInContextIO (insertActionKey regActionKey) appContext
          -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expDto = encode reqDto
          -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
      eitherUser <- runInContextIO (findUserById "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66") appContext
      eitherActionKeys <- runInContextIO findActionKeys appContext
          -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expDto}
      response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
      liftIO $ (isRight eitherUser) `shouldBe` True
      liftIO $ (isRight eitherActionKeys) `shouldBe` True
      let (Right userFromDb) = eitherUser
      let (Right actionKeys) = eitherActionKeys
      liftIO $ (userFromDb ^. active) `shouldBe` True
      liftIO $ Prelude.length actionKeys `shouldBe` 0
    createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "active"
    createNotFoundTest
      reqMethod
      "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0/state?hash="
      reqHeaders
      reqBody
      "user"
      "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
