module Wizard.Integration.Http.Common.HttpClient
  ( runRequest
  , runSimpleRequest
  ) where

import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Map (toList)
import qualified Data.Text as T
import Network.Wreq
  ( Part
  , Response
  , checkResponse
  , customPayloadMethodWith
  , defaults
  , getWith
  , headers
  , manager
  , partBS
  , responseBody
  , responseStatus
  , statusCode
  )

import LensesConfig hiding (headers)
import Shared.Constant.Component
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Logger

runRequest :: HttpRequest -> (Response BSL.ByteString -> Either AppError a) -> AppContextM a
runRequest req responseMapper = do
  logRequestMultipart req
  response <- runSimpleRequest req
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
        defaults & manager .~ Right httpClientManager & headers .~ reqHeaders & (checkResponse ?~ (\_ _ -> return ()))
  case req ^. multipartFileName of
    Just fileName -> liftIO . actionMultipart opts $ fileName
    Nothing -> liftIO . action $ opts
  where
    reqMethod = req ^. requestMethod
    reqUrl = req ^. requestUrl
    reqHeaders = mapHeader <$> toList (req ^. requestHeaders)
    action opts
      | reqMethod == "GET" = getWith opts reqUrl
      | otherwise = customPayloadMethodWith reqMethod opts reqUrl (req ^. requestBody)
    actionMultipart opts fileName =
      customPayloadMethodWith reqMethod opts reqUrl ([partBS (T.pack fileName) (req ^. requestBody)] :: [Part])

-- --------------------------------
-- PRIVATE
-- --------------------------------
mapHeader :: (String, String) -> (CI.CI BS.ByteString, BS.ByteString)
mapHeader (k, v) = (CI.mk . BS.pack $ k, BS.pack v)

-- --------------------------------
-- LOGGER
-- --------------------------------
logRequestMultipart request =
  case request ^. multipartFileName of
    Just fileName -> logInfoU _CMP_INTEGRATION ("Request Multipart FileName: '" ++ fileName ++ "'")
    Nothing -> logInfoU _CMP_INTEGRATION "Request Multipart: Not used"

logResponseErrorBody response =
  logInfoU _CMP_INTEGRATION ("Response Message: '" ++ show (response ^. responseBody) ++ "'")

logResponseBody eResDto =
  case eResDto of
    Right dto -> logInfoU _CMP_INTEGRATION "Response Body: Successfully parsed"
    Left error -> logWarnU _CMP_INTEGRATION "Response Body: Failed to parse"
