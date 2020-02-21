module Wizard.Specs.API.Common where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Foldable
import qualified Data.List as L
import Data.Maybe
import Data.Time
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import LensesConfig
import Shared.Api.Resource.Error.ErrorDTO ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api
import Shared.Localization.Messages.Public
import Wizard.Bootstrap.Web
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Service.Token.TokenService
import Wizard.Service.User.UserService
import Wizard.Util.List (elems)

import SharedTest.Specs.Common
import Wizard.Specs.Common

startWebApp :: AppContext -> IO Application
startWebApp appContext = do
  let baseContext =
        BaseContext
          { _baseContextAppConfig = appContext ^. applicationConfig
          , _baseContextLocalization = appContext ^. localization
          , _baseContextBuildInfoConfig = appContext ^. buildInfoConfig
          , _baseContextPool = appContext ^. pool
          , _baseContextMsgChannel = appContext ^. msgChannel
          , _baseContextHttpClientManager = appContext ^. httpClientManager
          }
  let config = appContext ^. applicationConfig
  let webPort = config ^. general . serverPort
  let env = config ^. general . environment
  return $ runMiddleware env $ runApp baseContext

reqAuthHeader :: Header
reqAuthHeader =
  ( "Authorization"
  , "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsImV4cCI6MjM5NDAwMzIzMSwidmVyc2lvbiI6IjEiLCJwZXJtaXNzaW9ucyI6WyJVTV9QRVJNIiwiT1JHX1BFUk0iLCJLTV9QRVJNIiwiS01fVVBHUkFERV9QRVJNIiwiS01fUFVCTElTSF9QRVJNIiwiUE1fUkVBRF9QRVJNIiwiUE1fV1JJVEVfUEVSTSIsIlFUTl9QRVJNIiwiRE1QX1BFUk0iXX0.6jt40r7YR-YBXMBmo3aKypQiE6ikrVTsU_bSKDn-gPk")

reqNonAdminAuthHeader :: Header
reqNonAdminAuthHeader =
  ( "Authorization"
  , "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyVXVpZCI6IjMwZDQ4Y2Y0LThjOGEtNDk2Zi1iYWZlLTU4NWJkMjM4Zjc5OCIsImV4cCI6MjQwMTYzNzU0MSwidmVyc2lvbiI6IjEiLCJwZXJtaXNzaW9ucyI6WyJLTV9QRVJNIiwiS01fVVBHUkFERV9QRVJNIiwiS01fUFVCTElTSF9QRVJNIiwiUE1fUkVBRF9QRVJNIiwiUVROX1BFUk0iLCJETVBfUEVSTSJdfQ.mTmVLO2AubdqaYNmjZlOMCqOAqZ5ULYXwLoEdwGCa1k")

reqAuthHeaderWithoutPerms :: AppConfig -> Permission -> Header
reqAuthHeaderWithoutPerms appConfig perm =
  let allPerms = getPermissionForRole appConfig "ADMIN"
      user = userAlbert & permissions .~ (L.delete perm allPerms)
      now = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
      token =
        createToken user now (appConfig ^. jwt ^. secret) (appConfig ^. jwt ^. version) (appConfig ^. jwt ^. expiration)
   in ("Authorization", BS.concat ["Bearer ", BS.pack token])

reqServiceHeader :: Header
reqServiceHeader = ("Authorization", "Bearer my-service-token")

reqCtHeader :: Header
reqCtHeader = contentTypeHeaderJSON

resCtHeaderPlain :: Header
resCtHeaderPlain = contentTypeHeaderJSON

resCtHeader = "Content-Type" <:> "application/json;charset=utf-8"

resCtHeaderJavascript = "Content-Type" <:> "application/javascript;charset=utf-8"

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

shouldRespondWith r matcher = do
  forM_ (match r matcher) (liftIO . expectationFailure)

createInvalidJsonTest reqMethod reqUrl reqBody missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
      -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createUserError _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
    let expBody = encode expDto
      -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createInvalidJsonArrayTest reqMethod reqUrl reqBody missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
      -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createUserError _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
    let expBody = encode expDto
      -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createAuthTest reqMethod reqUrl reqHeaders reqBody =
  it "HTTP 401 UNAUTHORIZED" $
    -- GIVEN: Prepare expectation
   do
    let expBody =
          [HJ.json|
    {
      status: 401,
      message: "Unable to get token"
    }
    |]
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expStatus = 401
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createNoPermissionTest appConfig reqMethod reqUrl otherHeaders reqBody missingPerm =
  it "HTTP 403 FORBIDDEN - no required permission" $
    -- GIVEN: Prepare request
   do
    let authHeader = reqAuthHeaderWithoutPerms appConfig missingPerm
    let reqHeaders = [authHeader] ++ otherHeaders
    -- GIVEN: Prepare expectation
    let expDto = createForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ missingPerm)
    let expBody = encode expDto
    let expHeaders = [resCtHeader] ++ resCorsHeaders
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
    let expHeaders = [resCtHeader] ++ resCorsHeaders
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
  liftIO $ (isRight eitherList) `shouldBe` True
  let (Right list) = eitherList
  liftIO $ (L.length list) `shouldBe` count

getFirstFromDB dbFunction appContext = do
  eitherList <- runInContextIO dbFunction appContext
  liftIO $ (isRight eitherList) `shouldBe` True
  let (Right list) = eitherList
  return $ list !! 0

getOneFromDB dbFunction appContext = do
  eitherOne <- runInContextIO dbFunction appContext
  liftIO $ (isRight eitherOne) `shouldBe` True
  let (Right one) = eitherOne
  return one
