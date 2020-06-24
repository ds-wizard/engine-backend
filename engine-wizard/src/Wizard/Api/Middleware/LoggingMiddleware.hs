module Wizard.Api.Middleware.LoggingMiddleware where

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
import Shared.Constant.Component
import Shared.Model.Config.Environment
import Shared.Util.Http (extractMethod, extractPath, findHeader, processHeaderInMiddleware)
import Shared.Util.Token
import Wizard.Service.Token.TokenService
import Wizard.Util.Logger

loggingMiddleware :: Environment -> Middleware
loggingMiddleware Test application request sendResponse = application request sendResponse
loggingMiddleware _ application request sendResponse =
  application request $ sendResponse . processHeaderInMiddleware logRequest request

logRequest :: Request -> Status -> [Header] -> [Header]
logRequest request resStatus resHeaders =
  filterOptionsRequests request resHeaders $
  unsafePerformIO $ do
    putStrLn $ createHttpLogRecord request resStatus resHeaders
    return resHeaders

createHttpLogRecord request resStatus resHeaders = L.intercalate "" ["[", showLogLevel logLevel, "] ", record]
  where
    logLevel :: LogLevel
    logLevel = statusToLogLevel resStatus
    logLevelS = statusToLogLevel resStatus
    mUserUuid :: Maybe String
    mUserUuid =
      case findHeader authorizationHeaderName (requestHeaders request) of
        Just headerValue -> extractUserUuid . T.pack $ headerValue
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

extractUserUuid :: T.Text -> Maybe String
extractUserUuid tokenHeader = separateToken (T.unpack tokenHeader) >>= getUserUuidFromToken

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
