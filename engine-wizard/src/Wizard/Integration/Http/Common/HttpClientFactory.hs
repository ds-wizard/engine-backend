module Wizard.Integration.Http.Common.HttpClientFactory where

import Control.Monad (when)

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS

--import Network.HTTP.Client (Manager, newManager, managerModifyResponse, Response, BodyReader)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import LensesConfig (debugLogHttpClient, general)
import Shared.Util.String (format, replace)
import Wizard.Model.Config.ServerConfig

createHttpClientManager :: ServerConfig -> IO Manager
createHttpClientManager serverConfig =
  let logHttpClient = serverConfig ^. general . debugLogHttpClient
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
  let headers = show $ requestHeaders request
  when
    logHttpClient
    (do putStrLn "-------------------------------------------------------------------------------------"
        logDebug $ format "Retrieving '%s://%s%s%s'" [protocol, h, p, q]
        logDebug $ format "Request Method '%s'" [m]
        logDebug $ format "Request Headers: '%s'" [headers]
        putStrLn "-----")

logResponse :: Bool -> Response BodyReader -> IO ()
logResponse logHttpClient response = do
  let status = responseStatus response
  let headers = responseHeaders response
  when
    logHttpClient
    (do logDebug "Retrieved Response"
        logDebug $ format "Response StatusCode: '%s'" [show status]
        logDebug $ format "Response Headers: '%s'" [show headers]
        putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

logDebug :: String -> IO ()
logDebug msg = putStrLn (format "[DEBUG][HTTP-CLIENT] %s" [msg])
