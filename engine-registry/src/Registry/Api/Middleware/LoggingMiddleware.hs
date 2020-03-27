module Registry.Api.Middleware.LoggingMiddleware where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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
import System.Console.Pretty (Color(..), color)
import System.IO.Unsafe

import Registry.Util.Logger (createOrgTokenLoggerStamp, createTraceUuidLoggerStamp)
import Shared.Constant.Api (authorizationHeaderName, xTraceUuidHeaderName)
import Shared.Model.Config.Environment
import Shared.Util.Http (extractMethod, extractPath, findHeader, processHeaderInMiddleware)
import Shared.Util.Token

loggingMiddleware :: Environment -> Middleware
loggingMiddleware Test application request sendResponse = application request $ sendResponse
loggingMiddleware _ application request sendResponse =
  application request $ sendResponse . (processHeaderInMiddleware logRequest request)

logRequest :: Request -> Status -> [Header] -> [Header]
logRequest request resStatus resHeaders =
  filterOptionsRequests request resHeaders $
  unsafePerformIO $ do
    putStrLn . (colorizeMessage resStatus) $ createLogMessage blockParts messageParts
    return resHeaders
  where
    blockParts = createBlockParts (requestHeaders request) resHeaders
    messageParts = createMessageParts request resStatus

createBlockParts :: [Header] -> [Header] -> [String]
createBlockParts reqHeaders resHeaders = [logLevel, userUuid, traceUuid]
  where
    logLevel = "[Rqst] "
    userUuid =
      case findHeader authorizationHeaderName reqHeaders of
        Just headerValue -> createOrgTokenLoggerStamp (extractOrgToken . T.pack $ headerValue)
        Nothing -> createOrgTokenLoggerStamp "---"
    traceUuid =
      case findHeader xTraceUuidHeaderName resHeaders of
        Just headerValue -> createTraceUuidLoggerStamp headerValue
        Nothing -> createTraceUuidLoggerStamp "---"

createMessageParts :: Request -> Status -> [String]
createMessageParts request resStatus = [status, method, path]
  where
    method = extractMethod request
    path = extractPath request
    status = statusToString resStatus

createLogMessage :: [String] -> [String] -> String
createLogMessage blockParts messageParts = intercalate "" blockParts ++ " " ++ unwords messageParts

extractOrgToken :: T.Text -> String
extractOrgToken tokenHeader =
  let orgTokenMaybe = separateToken (T.unpack tokenHeader)
   in fromMaybe (createOrgTokenLoggerStamp "---") orgTokenMaybe

colorizeMessage :: Status -> String -> String
colorizeMessage resStatus
  | resStatus == status200 = color Green
  | resStatus == status201 = color Green
  | resStatus == status202 = color Green
  | resStatus == status204 = color Green
  | resStatus == status302 = color Green
  | resStatus == status400 = color Magenta
  | resStatus == status401 = color Magenta
  | resStatus == status403 = color Magenta
  | resStatus == status404 = color Magenta
  | resStatus == status500 = color Red
  | otherwise = color Red

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

filterOptionsRequests request resHeaders callback =
  if extractMethod request == "OPTIONS"
    then resHeaders
    else callback
