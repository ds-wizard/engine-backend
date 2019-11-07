module Specs.API.Common where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Data.Either (isRight)
import Data.Foldable
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher
import Web.Scotty.Trans (scottyAppT)

import Api.Resource.Error.ErrorDTO ()
import Api.Router
import Constant.Api
import Database.Migration.Development.Statistics.Data.InstanceStatistics
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Context.BaseContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Util.List (elems)

import Specs.Common

startWebApp :: AppContext -> IO Application
startWebApp appContext = do
  let baseContext =
        BaseContext
        { _baseContextAppConfig = appContext ^. appConfig
        , _baseContextBuildInfoConfig = appContext ^. buildInfoConfig
        , _baseContextPool = appContext ^. pool
        }
      t m = runStdoutLoggingT $ runReaderT (runBaseContextM m) baseContext
  scottyAppT t (createEndpoints baseContext)

reqAdminAuthHeader :: Header
reqAdminAuthHeader = ("Authorization", "Bearer DswToken")

reqUserAuthHeader :: Header
reqUserAuthHeader = ("Authorization", "Bearer NetherlandsToken")

reqCtHeader :: Header
reqCtHeader = ("Content-Type", "application/json; charset=utf-8")

reqStatisticsHeader :: [Header]
reqStatisticsHeader =
  [ (CI.mk . BS.pack $ xDswUserCountHeaderName, BS.pack . show $ iStat ^. userCount)
  , (CI.mk . BS.pack $ xDswPkgCountHeaderName, BS.pack . show $ iStat ^. pkgCount)
  , (CI.mk . BS.pack $ xDswQtnCountHeaderName, BS.pack . show $ iStat ^. qtnCount)
  ]

resCtHeaderPlain :: Header
resCtHeaderPlain = ("Content-Type", "application/json; charset=utf-8")

resCtHeader = "Content-Type" <:> "application/json; charset=utf-8"

resCtHeaderJavascript = "Content-Type" <:> "application/javascript; charset=utf-8"

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
    let reqHeaders = [reqAdminAuthHeader, reqCtHeader]
      -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithErrorMessage $ "Error in $: key \"" ++ missingField ++ "\" not present"
    let expBody = encode expDto
      -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createInvalidJsonArrayTest reqMethod reqUrl reqBody missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAdminAuthHeader, reqCtHeader]
      -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithErrorMessage $ "Error in $[0]: key \"" ++ missingField ++ "\" not present"
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
      error: "Unauthorized",
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
createForbiddenTest reqMethod reqUrl reqHeaders reqBody forbiddenReason =
  it "HTTP 403 FORBIDDEN" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 403
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN forbiddenReason
    let expBody = encode expDto
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
    let expDto = NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator)
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
