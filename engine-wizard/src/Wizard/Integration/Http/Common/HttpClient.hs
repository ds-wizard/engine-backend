module Wizard.Integration.Http.Common.HttpClient (
  runRequest,
  runRequest',
  runSimpleRequest,
  mapHeader,
) where

import qualified Control.Exception.Base as E
import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Map (toList)
import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData (partContentType, partFilename)
import Network.Wreq (
  Part,
  Response,
  checkResponse,
  customPayloadMethodWith,
  defaults,
  getWith,
  headers,
  manager,
  partBS,
  responseBody,
  responseStatus,
  statusCode,
 )

import Shared.Constant.Component
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Logger

runRequest :: HttpRequest -> (Response BSL.ByteString -> Either AppError a) -> AppContextM a
runRequest req responseMapper = do
  logRequestMultipart req
  eResponse <- runSimpleRequest req
  case eResponse of
    Right response -> do
      let sc = response ^. responseStatus . statusCode
      if sc <= 399
        then do
          let eResDto = responseMapper response
          logResponseBody eResDto
          liftEither eResDto
        else do
          logResponseErrorBody response
          throwError . GeneralServerError $
            _ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR ("statusCode: " ++ show sc)
    Left error -> do
      logResponseErrorGeneral error
      throwError . GeneralServerError $ _ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "Request failed, see logs"

runRequest' :: HttpRequest -> (Response BSL.ByteString -> Either String a) -> AppContextM (Either String a)
runRequest' req responseMapper = do
  logRequestMultipart req
  eResponse <- runSimpleRequest req
  case eResponse of
    Right response -> do
      let sc = response ^. responseStatus . statusCode
      if sc <= 399
        then do
          let eResDto = responseMapper response
          logResponseBody eResDto
          return eResDto
        else do
          logResponseErrorBody response
          return . Left $
            f' "Request Failed\nStatus Code: %s\nResponse Body: %s" [show sc, show (response ^. responseBody)]
    Left error -> do
      logResponseErrorGeneral error
      return . Left $ f' "Request failed\nError: %s" [show error]

runSimpleRequest :: HttpRequest -> AppContextM (Either E.SomeException (Response BSL.ByteString))
runSimpleRequest req = do
  httpClientManager <- asks httpClientManager
  let opts =
        defaults & manager .~ Right httpClientManager & headers .~ reqHeaders & (checkResponse ?~ (\_ _ -> return ()))
  case req.multipart of
    Just multipart -> liftIO . E.try . actionMultipart opts $ multipart
    Nothing -> liftIO . E.try . action $ opts
  where
    reqMethod = req.requestMethod
    reqUrl = req.requestUrl
    reqHeaders = mapHeader <$> toList req.requestHeaders
    action opts
      | reqMethod == "GET" = getWith opts reqUrl
      | otherwise = customPayloadMethodWith reqMethod opts reqUrl req.requestBody
    actionMultipart opts fileConfig =
      customPayloadMethodWith reqMethod opts reqUrl ([(partBS (T.pack fileConfig.key) req.requestBody) {partFilename = fileConfig.fileName, partContentType = fmap BS.pack fileConfig.contentType}] :: [Part])

-- --------------------------------
-- PRIVATE
-- --------------------------------
mapHeader :: (String, String) -> (CI.CI BS.ByteString, BS.ByteString)
mapHeader (k, v) = (CI.mk . BS.pack $ k, BS.pack v)

-- --------------------------------
-- LOGGER
-- --------------------------------
logRequestMultipart request =
  case request.multipart of
    Just multipart -> logInfoU _CMP_INTEGRATION (f' "Request Multipart (key: '%s', fileName: '%s', contentType: '%s')" [multipart.key, show multipart.fileName, show multipart.contentType])
    Nothing -> logInfoU _CMP_INTEGRATION "Request Multipart: Not used"

logResponseErrorGeneral error = logInfoU _CMP_INTEGRATION (f' "Request failed: '%s'" [show error])

logResponseErrorBody response =
  logInfoU _CMP_INTEGRATION ("Response Message: '" ++ show (response ^. responseBody) ++ "'")

logResponseBody eResDto =
  case eResDto of
    Right dto -> logInfoU _CMP_INTEGRATION "Response Body: Successfully parsed"
    Left error -> logWarnU _CMP_INTEGRATION "Response Body: Failed to parse"
