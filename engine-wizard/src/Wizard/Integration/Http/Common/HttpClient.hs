module Wizard.Integration.Http.Common.HttpClient
  ( runRequest
  , runSimpleRequest
  ) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Map (toList)
import Network.Wreq
  ( Response
  , checkResponse
  , customPayloadMethodWith
  , defaults
  , getWith
  , headers
  , manager
  , responseBody
  , responseStatus
  , statusCode
  )

import LensesConfig hiding (headers)
import Shared.Model.Error.Error
import Wizard.Constant.Component
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Logger

runRequest :: HttpRequest -> (Response BSL.ByteString -> Either AppError a) -> AppContextM a
runRequest req responseMapper = do
  logRequest req
  response <- runSimpleRequest req
  logResponse response
  let sc = response ^. responseStatus . statusCode
  if sc <= 399
    then do
      let eResDto = responseMapper response
      logResponseBody eResDto
      liftEither eResDto
    else do
      logResponseErrorBody response
      throwError . GeneralServerError $ _ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR sc

runSimpleRequest :: HttpRequest -> AppContextM (Response BSL.ByteString)
runSimpleRequest req = do
  httpClientManager <- asks _appContextHttpClientManager
  let opts =
        defaults & manager .~ Right httpClientManager & headers .~ reqHeaders & checkResponse .~
        (Just $ \_ _ -> return ())
  liftIO . action $ opts
  where
    reqMethod = req ^. requestMethod
    reqUrl = req ^. requestUrl
    reqHeaders = mapHeader <$> (toList $ req ^. requestHeaders)
    reqBody = BS.pack $ req ^. requestBody
    action opts
      | reqMethod == "GET" && reqBody == "" = getWith opts reqUrl
      | otherwise = customPayloadMethodWith reqMethod opts reqUrl reqBody

-- --------------------------------
-- PRIVATE
-- --------------------------------
mapHeader :: (String, String) -> (CI.CI BS.ByteString, BS.ByteString)
mapHeader (k, v) = (CI.mk . BS.pack $ k, BS.pack v)

-- --------------------------------
-- LOGGER
-- --------------------------------
logRequest request = do
  logInfoU _CMP_INTEGRATION ("Retrieving '" ++ (request ^. requestUrl) ++ "'")
  logInfoU _CMP_INTEGRATION ("Request Method '" ++ (request ^. requestMethod) ++ "'")
  logInfoU _CMP_INTEGRATION ("Request Headers: '" ++ (show . toList $ request ^. requestHeaders) ++ "'")
  logInfoU _CMP_INTEGRATION ("Request Body: '" ++ (request ^. requestBody) ++ "'")

logResponse response = do
  logInfoU _CMP_INTEGRATION "Retrieved Response"
  logInfoU _CMP_INTEGRATION ("Response StatusCode: '" ++ show (response ^. responseStatus . statusCode) ++ "'")

logResponseErrorBody response =
  logInfoU _CMP_INTEGRATION ("Response Message: '" ++ show (response ^. responseBody) ++ "'")

logResponseBody eResDto =
  case eResDto of
    Right dto -> logInfoU _CMP_INTEGRATION "Response Body: Successfully parsed"
    Left error -> logWarnU _CMP_INTEGRATION "Response Body: Failed to parse"
