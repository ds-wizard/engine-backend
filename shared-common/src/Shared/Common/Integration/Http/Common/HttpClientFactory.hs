module Shared.Common.Integration.Http.Common.HttpClientFactory where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import Network.HTTP.Client (
  BodyReader,
  Manager,
  Request,
  RequestBody (..),
  Response,
  getOriginalRequest,
  host,
  managerModifyRequest,
  managerModifyResponse,
  method,
  newManager,
  path,
  queryString,
  requestBody,
  requestHeaders,
  responseHeaders,
  responseStatus,
  secure,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Shared.Common.Model.Config.ServerConfig hiding (requestBody, requestHeaders)
import Shared.Common.Util.Logger
import Shared.Common.Util.String (replace)

createHttpClientManager :: ServerConfigLogging -> IO Manager
createHttpClientManager serverConfig =
  let logHttpClient = serverConfig.httpClientDebug
   in newManager
        ( tlsManagerSettings
            { managerModifyRequest = modifyRequest logHttpClient
            , managerModifyResponse = modifyResponse logHttpClient
            }
        )

modifyRequest :: Bool -> Request -> IO Request
modifyRequest logHttpClient request = do
  let originalHeaders = requestHeaders request
      -- Filter out "User-Agent" headers (case-insensitive) and (re-)add our explicit "User-Agent" header, to ensure there's only one User-Agent header.
      -- Note: Reason for using case-insensitive search for header key(s) is because HTTP spec. states that header keys are case-insensitive.
      headersWithoutUA = filter (\(headerName, _) -> headerName /= CI.mk (BS.pack "User-Agent")) originalHeaders
      updatedRequest =
        request
          { path = BS.pack . replace "//" "/" . BS.unpack . path $ request
          , requestHeaders = ("User-Agent", "wizard-http-client") : headersWithoutUA
          }
  logRequest logHttpClient updatedRequest
  return updatedRequest

modifyResponse :: Bool -> Response BodyReader -> IO (Response BodyReader)
modifyResponse logHttpClient response = do
  logResponse logHttpClient response
  return response

-- ------------------------------------------------------------
-- Logging
-- ------------------------------------------------------------
logRequest :: Bool -> Request -> IO ()
logRequest logHttpClient request = do
  let m = BS.unpack . method $ request
  let protocol =
        if secure request
          then "https"
          else "http"
  let h = BS.unpack . host $ request
  let p = BS.unpack . path $ request
  let q = BS.unpack . queryString $ request
  let b =
        case requestBody request of
          RequestBodyLBS bytestring -> LBS.unpack bytestring
          RequestBodyBS bytestring -> BS.unpack bytestring
          _ -> "can't be shown"
  let headers = show $ requestHeaders request
  let mIdentity = getHeader "x-identity" request
  let mTraceUuid = getHeader "x-trace-uuid" request
  when
    logHttpClient
    ( do
        logMessage mIdentity mTraceUuid $ f' "Retrieving '%s://%s%s%s'" [protocol, h, p, q]
        logMessage mIdentity mTraceUuid $ f' "Request Method '%s'" [m]
        logMessage mIdentity mTraceUuid $ f' "Request Headers: '%s'" [headers]
        logMessage mIdentity mTraceUuid $ f' "Request Body '%s'" [b]
    )

logResponse :: Bool -> Response BodyReader -> IO ()
logResponse logHttpClient response = do
  let status = responseStatus response
  let headers = responseHeaders response
  let mIdentity = getHeader "x-identity" $ getOriginalRequest response
  let mTraceUuid = getHeader "x-trace-uuid" $ getOriginalRequest response
  when
    logHttpClient
    ( do
        logMessage mIdentity mTraceUuid "Retrieved Response"
        logMessage mIdentity mTraceUuid $ f' "Response StatusCode: '%s'" [show status]
        logMessage mIdentity mTraceUuid $ f' "Response Headers: '%s'" [show headers]
    )

logMessage :: Maybe String -> Maybe String -> String -> IO ()
logMessage mIdentity mTraceUuid msg = putStrLn (f' "[Debug] %s" [createLogRecord LevelDebug mIdentity mTraceUuid _CMP_HTTP_CLIENT msg])

getHeader :: String -> Request -> Maybe String
getHeader headerName request =
  fmap (\(_, value) -> BS.unpack value) . L.find (\(name, _) -> name == (CI.mk . BS.pack $ headerName)) $ request.requestHeaders
