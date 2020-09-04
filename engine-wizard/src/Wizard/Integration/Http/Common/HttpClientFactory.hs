module Wizard.Integration.Http.Common.HttpClientFactory where

import Control.Monad (when)

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client
  ( BodyReader
  , Manager
  , Request
  , RequestBody(..)
  , Response
  , host
  , managerModifyRequest
  , managerModifyResponse
  , method
  , newManager
  , path
  , queryString
  , requestBody
  , requestHeaders
  , responseHeaders
  , responseStatus
  , secure
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

import LensesConfig (httpClientDebug, logging)
import Shared.Util.String (replace)
import Wizard.Model.Config.ServerConfig
import Wizard.Util.Logger

createHttpClientManager :: ServerConfig -> IO Manager
createHttpClientManager serverConfig =
  let logHttpClient = serverConfig ^. logging . httpClientDebug
   in newManager
        (tlsManagerSettings
           {managerModifyRequest = modifyRequest logHttpClient, managerModifyResponse = modifyResponse logHttpClient})

modifyRequest :: Bool -> Request -> IO Request
modifyRequest logHttpClient request = do
  let updatedRequest = request {path = BS.pack . replace "//" "/" . BS.unpack . path $ request}
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
  when
    logHttpClient
    (do logMessage $ format "Retrieving '%s://%s%s%s'" [protocol, h, p, q]
        logMessage $ format "Request Method '%s'" [m]
        logMessage $ format "Request Headers: '%s'" [headers]
        logMessage $ format "Request Body '%s'" [b])

logResponse :: Bool -> Response BodyReader -> IO ()
logResponse logHttpClient response = do
  let status = responseStatus response
  let headers = responseHeaders response
  when
    logHttpClient
    (do logMessage "Retrieved Response"
        logMessage $ format "Response StatusCode: '%s'" [show status]
        logMessage $ format "Response Headers: '%s'" [show headers])

logMessage :: String -> IO ()
logMessage msg = putStrLn (format "[Debug] %s" [createLogRecord LevelDebug Nothing Nothing _CMP_HTTP_CLIENT msg])
