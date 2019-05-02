module Specs.API.UserAPISpec where

import Control.Lens
import Crypto.PasswordStore
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Data.Maybe
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.User.UserChangeDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserProfileChangeDTO
import Api.Resource.User.UserStateDTO
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.User.UserDAO
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Localization
import Model.ActionKey.ActionKey
import Model.Error.ErrorHelpers
import Service.User.UserMapper
import Service.User.UserService
import Util.List (elems)

import Specs.API.Common
import Specs.API.User.Detail_Password_Hash_PUT
import Specs.API.User.Detail_Password_PUT
import Specs.Common

userAPI appContext =
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. appConfig
    describe "USER API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /users
      -- ------------------------------------------------------------------------
     do
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
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "UM_PERM"
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
          liftIO $ (userFromDb ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (userFromDb ^. surname) `shouldBe` (reqDto ^. surname)
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
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Albert" } |] "surname"
        it "HTTP 400 BAD REQUEST if email is already registered" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                UserCreateDTO
                { _userCreateDTOName = "Albert"
                , _userCreateDTOSurname = "Einstein"
                , _userCreateDTOEmail = "albert.einstein@example.com"
                , _userCreateDTORole = Just "ADMIN"
                , _userCreateDTOPassword = "password"
                }
          let reqBody = encode reqDto
           -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)
          let expBody = encode expDto
           -- WHEN: Call APIA
          response <- request reqMethod reqUrl reqHeaders reqBody
           -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
      -- ------------------------------------------------------------------------
      -- GET /users/current
      -- ------------------------------------------------------------------------
      describe "GET /users/current" $ do
        let reqMethod = methodGet
        let reqUrl = "/users/current"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = toDTO userAlbert
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
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
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "UM_PERM"
        createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- PUT /users/current
      -- ------------------------------------------------------------------------
      describe "PUT /users/current" $ do
        let reqMethod = methodPut
        let reqUrl = "/users/current"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                UserProfileChangeDTO
                { _userProfileChangeDTOName = "EDITED: Isaac"
                , _userProfileChangeDTOSurname = "EDITED: Newton"
                , _userProfileChangeDTOEmail = "isaac.newton@example-edited.com"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
          let expDto =
                toDTO $
                ((userAlbert & name .~ (reqDto ^. name)) & surname .~ (reqDto ^. name)) & email .~ (reqDto ^. email)
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- runInContextIO (findUserByEmail (reqDto ^. email)) appContext
          -- AND: Compare response with expectation
          let (SResponse (Status status _) headers body) = response
          liftIO $ status `shouldBe` expStatus
          liftIO $ (expHeaders `elems` headers) `shouldBe` True
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` True
          let (Right userFromDb) = eitherUser
          liftIO $ (userFromDb ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (userFromDb ^. surname) `shouldBe` (reqDto ^. surname)
          liftIO $ (userFromDb ^. email) `shouldBe` (reqDto ^. email)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |] "name"
        it "HTTP 400 BAD REQUEST if email is already registered" $
         -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let johnUuid = fromJust . U.fromString $ "cb877c12-2654-41ae-a7b3-6f444d57af7f"
          let johnDto =
                UserCreateDTO
                { _userCreateDTOName = "John"
                , _userCreateDTOSurname = "Doe"
                , _userCreateDTOEmail = "john.doe@example.com"
                , _userCreateDTORole = Just "ADMIN"
                , _userCreateDTOPassword = "password"
                }
          runInContextIO (createUserByAdminWithUuid johnDto johnUuid) appContext
          let reqDto =
                UserProfileChangeDTO
                { _userProfileChangeDTOName = "EDITED: Isaac"
                , _userProfileChangeDTOSurname = "EDITED: Newton"
                , _userProfileChangeDTOEmail = "john.doe@example.com"
                }
          let reqBody = encode reqDto
           -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)
          let expBody = encode expDto
           -- WHEN: Call APIA
          response <- request reqMethod reqUrl reqHeaders reqBody
           -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- PUT /users/{userId}
      -- ------------------------------------------------------------------------
      describe "PUT /users/{userId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodPut
        let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto =
              UserChangeDTO
              { _userChangeDTOUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
              , _userChangeDTOName = "EDITED: Isaac"
              , _userChangeDTOSurname = "EDITED: Newton"
              , _userChangeDTOEmail = "isaac.newton@example-edited.com"
              , _userChangeDTORole = "ADMIN"
              , _userChangeDTOActive = True
              }
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
          liftIO $ (resBody ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (resBody ^. surname) `shouldBe` (reqDto ^. surname)
          liftIO $ (resBody ^. email) `shouldBe` (reqDto ^. email)
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` True
          let (Right userFromDb) = eitherUser
          liftIO $ (userFromDb ^. uuid) `shouldBe` (reqDto ^. uuid)
          liftIO $ (userFromDb ^. name) `shouldBe` (reqDto ^. name)
          liftIO $ (userFromDb ^. surname) `shouldBe` (reqDto ^. surname)
          liftIO $ (userFromDb ^. email) `shouldBe` (reqDto ^. email)
          liftIO $ (userFromDb ^. role) `shouldBe` (reqDto ^. role)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |] "name"
        it "HTTP 400 BAD REQUEST if email is already registered" $
         -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let johnUuid = fromJust . U.fromString $ "cb877c12-2654-41ae-a7b3-6f444d57af7f"
          let johnDto =
                UserCreateDTO
                { _userCreateDTOName = "John"
                , _userCreateDTOSurname = "Doe"
                , _userCreateDTOEmail = "john.doe@example.com"
                , _userCreateDTORole = Just "ADMIN"
                , _userCreateDTOPassword = "password"
                }
          runInContextIO (createUserByAdminWithUuid johnDto johnUuid) appContext
          let reqDto =
                UserChangeDTO
                { _userChangeDTOUuid = johnUuid
                , _userChangeDTOName = "EDITED: Isaac"
                , _userChangeDTOSurname = "EDITED: Newton"
                , _userChangeDTOEmail = "albert.einstein@example.com"
                , _userChangeDTORole = "ADMIN"
                , _userChangeDTOActive = True
                }
          let reqBody = encode reqDto
           -- AND: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)
          let expBody = encode expDto
           -- WHEN: Call APIA
          response <- request reqMethod "/users/cb877c12-2654-41ae-a7b3-6f444d57af7f" reqHeaders reqBody
           -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "UM_PERM"
        createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- PUT /users/current/password
      -- ------------------------------------------------------------------------
      describe "PUT /users/current/password" $ do
        let reqMethod = methodPut
        let reqUrl = "/users/current/password"
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto = UserPasswordDTO {_userPasswordDTOPassword = "newPassword"}
          let reqBody = encode reqDto
          -- AND: Prepare expectation
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- runInContextIO (findUserById (U.toString $ userAlbert ^. uuid)) appContext
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` True
          let (Right userFromDb) = eitherUser
          let isSame = verifyPassword (BS.pack (reqDto ^. password)) (BS.pack (userFromDb ^. passwordHash))
          liftIO $ isSame `shouldBe` True
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"
        createAuthTest reqMethod reqUrl [] ""
      detail_password_put appContext
      detail_password_hash_put appContext
      -- ------------------------------------------------------------------------
      -- PUT /users/{userId}/state?hash={hash}
      -- ------------------------------------------------------------------------
      describe "PUT /users/{userId}/state?hash={hash}" $
        -- GIVEN:Prepare request
       do
        let reqMethod = methodPut
        let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
        let reqHeaders = [reqCtHeader]
        let reqDto = UserStateDTO {_userStateDTOActive = True}
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $
          -- AND: Prepare DB
         do
          let actionKey =
                ActionKey
                { _actionKeyUuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
                , _actionKeyUserId = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                , _actionKeyAType = RegistrationActionKey
                , _actionKeyHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
                , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
                }
          eitherActionKey <- runInContextIO (insertActionKey actionKey) appContext
          -- AND: Prepare expectation
          let expStatus = 200
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
        createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0/state?hash=" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- DELETE /users/{userId}
      -- ------------------------------------------------------------------------
      describe "DELETE /users/{userId}" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodDelete
        let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 204 NO CONTENT" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 204
          let expHeaders = resCorsHeaders
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- runInContextIO (findUserById "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66") appContext
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` False
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "UM_PERM"
        createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
