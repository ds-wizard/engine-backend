module Wizard.Specs.API.Common where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import Data.Foldable
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api
import Shared.Localization.Messages.Public
import Shared.Util.List (elems)
import Wizard.Bootstrap.Web
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Service.User.UserService

import SharedTest.Specs.Common
import Wizard.Specs.Common

startWebApp :: AppContext -> IO Application
startWebApp appContext = do
  let baseContext =
        BaseContext
          { _baseContextServerConfig = appContext ^. serverConfig
          , _baseContextLocalization = appContext ^. localization
          , _baseContextBuildInfoConfig = appContext ^. buildInfoConfig
          , _baseContextPool = appContext ^. pool
          , _baseContextMsgChannel = appContext ^. msgChannel
          , _baseContextHttpClientManager = appContext ^. httpClientManager
          , _baseContextRegistryClient = appContext ^. registryClient
          , _baseContextCache = appContext ^. cache
          }
  let config = appContext ^. serverConfig
  let webPort = config ^. general . serverPort
  let env = config ^. general . environment
  return $ runMiddleware env $ runApp baseContext

reqAuthToken :: String
reqAuthToken =
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsImV4cCI6MjQ1Njk4MjMyMCwidmVyc2lvbiI6IjIifQ.QFwvynp-TcEckL2wIQ5hgX71DVrsUMWrSTY9uzADnLU"

reqAuthHeader :: Header
reqAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ reqAuthToken)

reqNonAdminAuthToken :: String
reqNonAdminAuthToken =
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyVXVpZCI6IjMwZDQ4Y2Y0LThjOGEtNDk2Zi1iYWZlLTU4NWJkMjM4Zjc5OCIsImV4cCI6MjQ1Njk4MjI1OSwidmVyc2lvbiI6IjIifQ.bk1qLv7CTgROvc9ncTV5WD9sdYb5qEabIlWss7lQ87k"

reqNonAdminAuthHeader :: Header
reqNonAdminAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ reqNonAdminAuthToken)

userWithoutPerm :: ServerConfig -> Permission -> User
userWithoutPerm serverConfig perm =
  let allPerms = getPermissionForRole serverConfig _USER_ROLE_ADMIN
   in userAlbert & permissions .~ L.delete perm allPerms

reqServiceHeader :: Header
reqServiceHeader = ("Authorization", "Bearer my-service-token")

reqCtHeader :: Header
reqCtHeader = contentTypeHeaderJSON

resCtHeaderPlain :: Header
resCtHeaderPlain = contentTypeHeaderJSON

resCtHeader = "Content-Type" <:> "application/json"

resCtHeaderUtf8 = "Content-Type" <:> "application/json;charset=utf-8"

resCtHeaderJavascript = "Content-Type" <:> "application/javascript"

resCorsHeadersPlain :: [Header]
resCorsHeadersPlain =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Credential", "true")
  , ("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization")
  , ("Access-Control-Allow-Methods", "OPTIONS, HEAD, GET, POST, PUT, DELETE")
  ]

resCorsHeaders =
  [ "Access-Control-Allow-Origin" <:> "*"
  , "Access-Control-Allow-Credential" <:> "true"
  , "Access-Control-Allow-Headers" <:> "Origin, X-Requested-With, Content-Type, Accept, Authorization"
  , "Access-Control-Allow-Methods" <:> "OPTIONS, HEAD, GET, POST, PUT, DELETE"
  ]

shouldRespondWith r matcher = forM_ (match r matcher) (liftIO . expectationFailure)

createInvalidJsonTest reqMethod reqUrl missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqBody = BSL.pack "{}"
      -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeaderUtf8 : resCorsHeaders
    let expDto = createUserError _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
    let expBody = encode expDto
      -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createAuthTest reqMethod reqUrl reqHeaders reqBody =
  it "HTTP 401 UNAUTHORIZED" $
    -- GIVEN: Prepare expectation
   do
    let expStatus = 401
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UnauthorizedErrorDTO "Unable to get token"
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createNoPermissionTest appContext reqMethod reqUrl otherHeaders reqBody missingPerm =
  it "HTTP 403 FORBIDDEN - no required permission" $
    -- GIVEN: Prepare request
   do
    let user = userWithoutPerm (appContext ^. serverConfig) missingPerm
    runInContextIO (updateUserById user) appContext
    let reqHeaders = reqAuthHeader : otherHeaders
    -- GIVEN: Prepare expectation
    let expDto = createForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ missingPerm)
    let expBody = encode expDto
    let expHeaders = resCtHeader : resCorsHeaders
    let expStatus = 403
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createNotFoundTest reqMethod reqUrl reqHeaders reqBody entityName identificator =
  it "HTTP 404 NOT FOUND - entity doesn't exist" $
      -- GIVEN: Prepare expectation
   do
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createNotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator)
    let expBody = encode expDto
      -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

assertResStatus resStatus expStatus = liftIO $ resStatus `shouldBe` expStatus

assertResHeaders resHeaders expHeaders = liftIO $ (expHeaders `elems` resHeaders) `shouldBe` True

destructResponse response =
  let (SResponse (Status status _) headers body) = response
      (Right resBody) = eitherDecode body
   in (status, headers, resBody)

assertCountInDB dbFunction appContext count = do
  eitherList <- runInContextIO dbFunction appContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  liftIO $ L.length list `shouldBe` count

getFirstFromDB dbFunction appContext = do
  eitherList <- runInContextIO dbFunction appContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  return . head $ list

getOneFromDB dbFunction appContext = do
  eitherOne <- runInContextIO dbFunction appContext
  liftIO $ isRight eitherOne `shouldBe` True
  let (Right one) = eitherOne
  return one
