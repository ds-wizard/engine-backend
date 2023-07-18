module Shared.Common.Integration.Http.Common.HttpClient (
  runRequest,
  runRequest',
  runSimpleRequest,
  mapHeader,
) where

import qualified Control.Exception.Base as E
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Map (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import GHC.Records
import Network.HTTP.Client (Manager (..), responseBody, responseStatus)
import Network.HTTP.Client.MultipartFormData (partContentType, partFilename)
import Network.HTTP.Types.Status (statusCode)
import Network.Wreq (
  Part,
  Response,
  customPayloadMethodWith,
  defaults,
  getWith,
  partBS,
 )
import Network.Wreq.Types (Options (..))

import Shared.Common.Constant.Component
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error
import Shared.Common.Model.Http.HttpRequest
import Shared.Common.Util.Logger

runRequest
  :: ( MonadReader s m
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "httpClientManager'" s Manager
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => HttpRequest
  -> (Response BSL.ByteString -> Either AppError a)
  -> m a
runRequest req responseMapper = do
  logRequestMultipart req
  eResponse <- runSimpleRequest req
  case eResponse of
    Right response -> do
      let sc = statusCode . responseStatus $ response
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

runRequest'
  :: ( MonadReader s m
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "httpClientManager'" s Manager
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => HttpRequest
  -> (Response BSL.ByteString -> Either String a)
  -> m (Either String a)
runRequest' req responseMapper = do
  logRequestMultipart req
  eResponse <- runSimpleRequest req
  case eResponse of
    Right response -> do
      let sc = statusCode . responseStatus $ response
      if sc <= 399
        then do
          let eResDto = responseMapper response
          logResponseBody eResDto
          return eResDto
        else do
          logResponseErrorBody response
          return . Left $
            f' "Request Failed\nStatus Code: %s\nResponse Body: %s" [show sc, show . responseBody $ response]
    Left error -> do
      logResponseErrorGeneral error
      return . Left $ f' "Request failed\nError: %s" [show error]

runSimpleRequest
  :: ( MonadReader s m
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "httpClientManager'" s Manager
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => HttpRequest
  -> m (Either E.SomeException (Response BSL.ByteString))
runSimpleRequest req = do
  context <- ask
  let httpClientManager = context.httpClientManager'
  let opts =
        defaults
          { manager = Right httpClientManager
          , headers = reqHeaders context
          , checkResponse = Just (\_ _ -> return ())
          }
  case req.multipart of
    Just multipart -> liftIO . E.try . actionMultipart opts $ multipart
    Nothing -> liftIO . E.try . action $ opts
  where
    reqMethod = req.requestMethod
    reqUrl = req.requestUrl
    reqHeaders context = mapHeader <$> (toList req.requestHeaders ++ [("x-identity", fromMaybe "------------------------------------" context.identity'), ("x-trace-uuid", U.toString context.traceUuid')])
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
    Just multipart -> logInfoI _CMP_INTEGRATION (f' "Request Multipart (key: '%s', fileName: '%s', contentType: '%s')" [multipart.key, show multipart.fileName, show multipart.contentType])
    Nothing -> logInfoI _CMP_INTEGRATION "Request Multipart: Not used"

logResponseErrorGeneral error = logInfoI _CMP_INTEGRATION (f' "Request failed: '%s'" [show error])

logResponseErrorBody response =
  logInfoI _CMP_INTEGRATION ("Response Message: '" ++ show (responseBody response) ++ "'")

logResponseBody eResDto =
  case eResDto of
    Right dto -> logInfoI _CMP_INTEGRATION "Response Body: Successfully parsed"
    Left error -> logWarnI _CMP_INTEGRATION "Response Body: Failed to parse"
