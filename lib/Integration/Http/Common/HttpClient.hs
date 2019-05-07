module Integration.Http.Common.HttpClient
  ( runRequest
  , runSimpleRequest
  ) where

import Control.Lens ((^.))
import Control.Lens ((&), (.~))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Map (toList)
import Network.Wreq
       (Response, customPayloadMethodWith, defaults, headers, manager,
        responseStatus, statusCode)

import Constant.Component
import LensesConfig hiding (headers)
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Http.HttpRequest
import Util.Logger (logInfoU, logWarnU, msg)

runRequest :: HttpRequest -> (Response BSL.ByteString -> Either AppError a) -> AppContextM (Either AppError a)
runRequest req responseMapper = do
  logRequest req
  response <- runSimpleRequest req
  logResponse response
  if (response ^. responseStatus . statusCode) <= 399
    then do
      let eResDto = responseMapper response
      logResponseBody eResDto
      return eResDto
    else return . Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR

runSimpleRequest :: HttpRequest -> AppContextM (Response BSL.ByteString)
runSimpleRequest req = do
  httpClientManager <- asks _appContextHttpClientManager
  let opts = defaults & manager .~ Right httpClientManager & headers .~ reqHeaders
  liftIO $ customPayloadMethodWith reqMethod opts reqUrl reqBody
  where
    reqMethod = req ^. requestMethod
    reqUrl = req ^. requestUrl
    reqHeaders = mapHeader <$> (toList $ req ^. requestHeaders)
    reqBody = BS.pack $ req ^. requestBody

-- --------------------------------
-- PRIVATE
-- --------------------------------
mapHeader :: (String, String) -> (CI.CI BS.ByteString, BS.ByteString)
mapHeader (k, v) = (CI.mk . BS.pack $ k, BS.pack v)

-- --------------------------------
-- LOGGER
-- --------------------------------
logRequest request = do
  logInfoU $ msg _CMP_INTEGRATION ("Retrieving '" ++ (request ^. requestUrl) ++ "'")
  logInfoU $ msg _CMP_INTEGRATION ("Request Headers: '" ++ (show . toList $ request ^. requestHeaders) ++ "'")
  logInfoU $ msg _CMP_INTEGRATION ("Request Body: '" ++ (request ^. requestBody) ++ "'")

logResponse response = do
  logInfoU $ msg _CMP_INTEGRATION "Retrieved Response"
  logInfoU $ msg _CMP_INTEGRATION ("Response StatusCode: '" ++ (show $ response ^. responseStatus . statusCode) ++ "'")

logResponseBody eResDto =
  case eResDto of
    Right dto -> logInfoU $ msg _CMP_INTEGRATION "Response Body: Successfully parsed"
    Left error -> logWarnU $ msg _CMP_INTEGRATION "Response Body: Failed to parse"
