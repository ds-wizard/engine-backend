module Specs.API.UserAPISpec where

import Control.Lens
import Crypto.PasswordStore
import Data.Aeson
import Data.Aeson (Value(..), (.=), object)
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
import Common.Error
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.User.UserDAO
import Model.ActionKey.ActionKey
import Model.User.User
import Service.ActionKey.ActionKeyService
import Service.User.UserService

import Specs.API.Common
import Specs.Common

userAPI context dswConfig =
  with (startWebApp context dswConfig) $ do
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
          let expDto =
                [ UserDTO
                  { _udtoUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                  , _udtoName = "Darth"
                  , _udtoSurname = "Vader"
                  , _udtoEmail = "darth.vader@deathstar.com"
                  , _udtoRole = "ADMIN"
                  , _udtoPermissions =
                      [ "UM_PERM"
                      , "ORG_PERM"
                      , "KM_PERM"
                      , "KM_UPGRADE_PERM"
                      , "KM_PUBLISH_PERM"
                      , "PM_PERM"
                      , "WIZ_PERM"
                      , "DMP_PERM"
                      ]
                  , _udtoIsActive = True
                  }
                ]
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
          liftIO $ deleteActionKeys context
          -- AND: Prepare request
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                UserCreateDTO
                { _ucdtoName = "John"
                , _ucdtoSurname = "Doe"
                , _ucdtoEmail = "john.doe@example.com"
                , _ucdtoRole = Just "ADMIN"
                , _ucdtoPassword = "password"
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 201
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto =
                UserDTO
                { _udtoUuid = U.nil
                , _udtoName = reqDto ^. ucdtoName
                , _udtoSurname = reqDto ^. ucdtoSurname
                , _udtoEmail = reqDto ^. ucdtoEmail
                , _udtoRole = fromJust $ reqDto ^. ucdtoRole
                , _udtoPermissions =
                    [ "UM_PERM"
                    , "ORG_PERM"
                    , "KM_PERM"
                    , "KM_UPGRADE_PERM"
                    , "KM_PUBLISH_PERM"
                    , "PM_PERM"
                    , "WIZ_PERM"
                    , "DMP_PERM"
                    ]
                , _udtoIsActive = False
                }
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- liftIO $ findUserByEmail context (reqDto ^. ucdtoEmail)
          liftIO $ (isRight eitherUser) `shouldBe` True
          let (Right userFromDb) = eitherUser
          let expBody = encode (expDto & udtoUuid .~ (userFromDb ^. uUuid))
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (userFromDb ^. uName) `shouldBe` (reqDto ^. ucdtoName)
          liftIO $ (userFromDb ^. uSurname) `shouldBe` (reqDto ^. ucdtoSurname)
          liftIO $ (userFromDb ^. uEmail) `shouldBe` (reqDto ^. ucdtoEmail)
          liftIO $ (userFromDb ^. uRole) `shouldBe` fromJust (reqDto ^. ucdtoRole)
          liftIO $ (userFromDb ^. uPermissions) `shouldBe` (expDto ^. udtoPermissions)
          liftIO $ (userFromDb ^. uIsActive) `shouldBe` (expDto ^. udtoIsActive)
          -- THEN: Check created action Key
          eitherActionKeys <- liftIO $ findActionKeys context
          liftIO $ (isRight eitherActionKeys) `shouldBe` True
          let (Right actionKeys) = eitherActionKeys
          liftIO $ Prelude.length actionKeys `shouldBe` 1
          let actionKey = actionKeys !! 00
          liftIO $ (actionKey ^. akUserId) `shouldBe` (userFromDb ^. uUuid)
          liftIO $ (actionKey ^. akType) `shouldBe` RegistrationActionKey
          liftIO $ Prelude.length (actionKey ^. akHash) > 0 `shouldBe` True
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Darth" } |] "surname"
        it "HTTP 400 BAD REQUEST if email is already registered" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let reqDto =
                UserCreateDTO
                { _ucdtoName = "Darth"
                , _ucdtoSurname = "Vader"
                , _ucdtoEmail = "darth.vader@deathstar.com"
                , _ucdtoRole = Just "ADMIN"
                , _ucdtoPassword = "password"
                }
          let reqBody = encode reqDto
           -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("email", "User with given email is already exists")
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
          let expDto =
                UserDTO
                { _udtoUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                , _udtoName = "Darth"
                , _udtoSurname = "Vader"
                , _udtoEmail = "darth.vader@deathstar.com"
                , _udtoRole = "ADMIN"
                , _udtoPermissions =
                    [ "UM_PERM"
                    , "ORG_PERM"
                    , "KM_PERM"
                    , "KM_UPGRADE_PERM"
                    , "KM_PUBLISH_PERM"
                    , "PM_PERM"
                    , "WIZ_PERM"
                    , "DMP_PERM"
                    ]
                , _udtoIsActive = True
                }
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
          let expDto =
                UserDTO
                { _udtoUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                , _udtoName = "Darth"
                , _udtoSurname = "Vader"
                , _udtoEmail = "darth.vader@deathstar.com"
                , _udtoRole = "ADMIN"
                , _udtoPermissions =
                    [ "UM_PERM"
                    , "ORG_PERM"
                    , "KM_PERM"
                    , "KM_UPGRADE_PERM"
                    , "KM_PUBLISH_PERM"
                    , "PM_PERM"
                    , "WIZ_PERM"
                    , "DMP_PERM"
                    ]
                , _udtoIsActive = True
                }
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
                UserDTO
                { _udtoUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                , _udtoName = "EDITED: John"
                , _udtoSurname = "EDITED: Doe"
                , _udtoEmail = "EDITED: john.doe@example.com"
                , _udtoRole = "ADMIN"
                , _udtoPermissions = ["UM_PERM", "ORG_PERM", "KM_PERM", "KM_UPGRADE_PERM"]
                , _udtoIsActive = True
                }
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- liftIO $ findUserByEmail context (reqDto ^. udtoEmail)
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` True
          let (Right userFromDb) = eitherUser
          liftIO $ (userFromDb ^. uUuid) `shouldBe` (reqDto ^. udtoUuid)
          liftIO $ (userFromDb ^. uName) `shouldBe` (reqDto ^. udtoName)
          liftIO $ (userFromDb ^. uSurname) `shouldBe` (reqDto ^. udtoSurname)
          liftIO $ (userFromDb ^. uEmail) `shouldBe` (reqDto ^. udtoEmail)
          liftIO $ (userFromDb ^. uRole) `shouldBe` (reqDto ^. udtoRole)
          liftIO $ (userFromDb ^. uPermissions) `shouldBe` (reqDto ^. udtoPermissions)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |] "name"
        it "HTTP 400 BAD REQUEST if email is already registered" $
         -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let johnUuid = fromJust . U.fromString $ "cb877c12-2654-41ae-a7b3-6f444d57af7f"
          let johnDto =
                UserCreateDTO
                { _ucdtoName = "John"
                , _ucdtoSurname = "Doe"
                , _ucdtoEmail = "john.doe@example.com"
                , _ucdtoRole = Just "ADMIN"
                , _ucdtoPassword = "password"
                }
          liftIO $ createUserWithGivenUuid context dswConfig johnUuid johnDto True
          let reqDto =
                UserDTO
                { _udtoUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                , _udtoName = "EDITED: John"
                , _udtoSurname = "EDITED: Doe"
                , _udtoEmail = "john.doe@example.com"
                , _udtoRole = "ADMIN"
                , _udtoPermissions = ["UM_PERM", "ORG_PERM", "KM_PERM", "KM_UPGRADE_PERM"]
                , _udtoIsActive = True
                }
          let reqBody = encode reqDto
           -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("email", "User with given email is already exists")
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
              UserDTO
              { _udtoUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
              , _udtoName = "EDITED: John"
              , _udtoSurname = "EDITED: Doe"
              , _udtoEmail = "darth.vader@deathstar.com"
              , _udtoRole = "ADMIN"
              , _udtoPermissions = ["UM_PERM", "ORG_PERM", "KM_PERM", "KM_UPGRADE_PERM"]
              , _udtoIsActive = True
              }
        let reqBody = encode reqDto
        it "HTTP 200 OK" $
          -- GIVEN: Prepare expectation
         do
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = reqDto
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- liftIO $ findUserByEmail context (reqDto ^. udtoEmail)
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` True
          let (Right userFromDb) = eitherUser
          liftIO $ (userFromDb ^. uUuid) `shouldBe` (reqDto ^. udtoUuid)
          liftIO $ (userFromDb ^. uName) `shouldBe` (reqDto ^. udtoName)
          liftIO $ (userFromDb ^. uSurname) `shouldBe` (reqDto ^. udtoSurname)
          liftIO $ (userFromDb ^. uEmail) `shouldBe` (reqDto ^. udtoEmail)
          liftIO $ (userFromDb ^. uRole) `shouldBe` (reqDto ^. udtoRole)
          liftIO $ (userFromDb ^. uPermissions) `shouldBe` (reqDto ^. udtoPermissions)
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { uuid: "91a64ea5-55e1-4445-918d-e3f5534362f4" } |] "name"
        it "HTTP 400 BAD REQUEST if email is already registered" $
         -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          let johnUuid = fromJust . U.fromString $ "cb877c12-2654-41ae-a7b3-6f444d57af7f"
          let johnDto =
                UserCreateDTO
                { _ucdtoName = "John"
                , _ucdtoSurname = "Doe"
                , _ucdtoEmail = "john.doe@example.com"
                , _ucdtoRole = Just "ADMIN"
                , _ucdtoPassword = "password"
                }
          liftIO $ createUserWithGivenUuid context dswConfig johnUuid johnDto True
          let reqDto =
                UserDTO
                { _udtoUuid = johnUuid
                , _udtoName = "EDITED: John"
                , _udtoSurname = "EDITED: Doe"
                , _udtoEmail = "darth.vader@deathstar.com"
                , _udtoRole = "ADMIN"
                , _udtoPermissions = ["UM_PERM", "ORG_PERM", "KM_PERM", "KM_UPGRADE_PERM"]
                , _udtoIsActive = True
                }
          let reqBody = encode reqDto
           -- GIVEN: Prepare expectation
          let expStatus = 400
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = createErrorWithFieldError ("email", "User with given email is already exists")
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
          let reqDto = UserPasswordDTO {_updtoPassword = "newPassword"}
          let reqBody = encode reqDto
          -- GIVEN: Prepare expectation
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
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"
        createAuthTest reqMethod reqUrl [] ""
      -- ------------------------------------------------------------------------
      -- PUT /users/{userId}/password
      -- ------------------------------------------------------------------------
      describe "PUT /users/{userId}/password" $
        -- GIVEN: Prepare request
       do
        let reqMethod = methodPut
        let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto = UserPasswordDTO {_updtoPassword = "newPassword"}
        let reqBody = encode reqDto
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
        createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "UM_PERM"
        createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0/password" reqHeaders reqBody
      -- ------------------------------------------------------------------------
      -- PUT /users/{userId}/state?hash={hash}
      -- ------------------------------------------------------------------------
      describe "/users/{userId}/state?hash={hash}" $
        -- GIVEN:Prepare request
       do
        let reqMethod = methodPut
        let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqDto = UserStateDTO {_usdtoActive = True}
        let reqBody = encode reqDto
        it "HTTP 204 NO CONTENT" $
          -- AND: Prepare DB
         do
          let actionKey =
                ActionKey
                { _akUuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
                , _akUserId = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
                , _akType = RegistrationActionKey
                , _akHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
                }
          eitherActionKey <- liftIO $ insertActionKey context actionKey
          -- AND: Prepare expectation
          let expStatus = 200
          let expHeaders = resCorsHeaders
          let expDto = encode reqDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- THEN: Find a result
          eitherUser <- liftIO $ findUserById context "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
          eitherActionKeys <- liftIO $ findActionKeys context
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expDto}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` True
          liftIO $ (isRight eitherActionKeys) `shouldBe` True
          let (Right userFromDb) = eitherUser
          let (Right actionKeys) = eitherActionKeys
          liftIO $ (userFromDb ^. uIsActive) `shouldBe` True
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
          eitherUser <- liftIO $ findUserById context "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
          response `shouldRespondWith` responseMatcher
          -- AND: Compare state in DB with expectation
          liftIO $ (isRight eitherUser) `shouldBe` False
        createAuthTest reqMethod reqUrl [] ""
        createNoPermissionTest dswConfig reqMethod reqUrl [] "" "UM_PERM"
        createNotFoundTest reqMethod "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0" reqHeaders reqBody
