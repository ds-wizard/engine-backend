module Shared.Api.Middleware.LoggingMiddleware where

import qualified Data.List as L
import qualified Data.Text as T
import Network.HTTP.Types
  ( Header
  , Status
  , status200
  , status201
  , status202
  , status204
  , status302
  , status400
  , status401
  , status403
  , status404
  , status500
  )
import Network.Wai (Middleware, Request(..))
import System.IO.Unsafe

import Shared.Constant.Api (authorizationHeaderName, xTraceUuidHeaderName)
import Shared.Model.Config.Environment
import Shared.Util.Http (extractMethod, extractPath, findHeader, processHeaderInMiddleware)
import Shared.Util.Logger

createLoggingMiddleware :: (T.Text -> Maybe String) -> Environment -> Middleware
createLoggingMiddleware extractIdentity Test application request sendResponse = application request sendResponse
createLoggingMiddleware extractIdentity _ application request sendResponse =
  application request $ sendResponse . processHeaderInMiddleware (logRequest extractIdentity) request

logRequest :: (T.Text -> Maybe String) -> Request -> Status -> [Header] -> [Header]
logRequest extractIdentity request resStatus resHeaders =
  filterOptionsRequests request resHeaders $
  unsafePerformIO $ do
    putStrLn $ createHttpLogRecord extractIdentity request resStatus resHeaders
    return resHeaders

createHttpLogRecord extractIdentity request resStatus resHeaders =
  L.intercalate "" ["[", showLogLevel logLevel, "] ", record]
  where
    logLevel :: LogLevel
    logLevel = statusToLogLevel resStatus
    logLevelS = statusToLogLevel resStatus
    mUserUuid :: Maybe String
    mUserUuid =
      case findHeader authorizationHeaderName (requestHeaders request) of
        Just headerValue -> extractIdentity . T.pack $ headerValue
        Nothing -> Nothing
    mTraceUuid :: Maybe String
    mTraceUuid = findHeader xTraceUuidHeaderName resHeaders
    message :: String
    message = unwords (createMessageParts request resStatus)
    record :: String
    record = createLogRecord logLevel mUserUuid mTraceUuid _CMP_API message

createMessageParts :: Request -> Status -> [String]
createMessageParts request resStatus = [status, method, path]
  where
    method = extractMethod request
    path = extractPath request
    status = statusToString resStatus

statusToString :: Status -> String
statusToString resStatus
  | resStatus == status200 = "200 OK"
  | resStatus == status201 = "201 Created"
  | resStatus == status202 = "202 Accepted"
  | resStatus == status204 = "204 No Content"
  | resStatus == status302 = "302 Found"
  | resStatus == status400 = "400 Bad Request"
  | resStatus == status401 = "401 Unauthorized"
  | resStatus == status403 = "403 Forbidden"
  | resStatus == status404 = "404 Not Found"
  | resStatus == status500 = "500 Internal Server Error"
  | otherwise = show resStatus

statusToLogLevel :: Status -> LogLevel
statusToLogLevel resStatus
  | resStatus == status400 = LevelWarn
  | resStatus == status401 = LevelWarn
  | resStatus == status403 = LevelWarn
  | resStatus == status404 = LevelWarn
  | resStatus == status500 = LevelError
  | otherwise = LevelInfo

filterOptionsRequests request resHeaders callback =
  if extractMethod request == "OPTIONS"
    then resHeaders
    else callback
