module Wizard.Api.Middleware.AuthMiddleware where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (mk)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Method (methodOptions)
import Network.Wai (Middleware, Request, ResponseReceived, pathInfo, requestHeaders, requestMethod)
import Prelude hiding (exp)
import Text.Regex

import Shared.Constant.Api (authorizationHeaderName)
import Shared.Util.Token
import Wizard.Api.Handler.Common
import Wizard.LensesConfig hiding (requestHeaders, requestMethod)
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.AppConfig
import Wizard.Service.Token.TokenService

type EndpointDefinition = (H.Method, Regex)

getRequestURL :: Request -> String
getRequestURL request = T.unpack . (T.intercalate "/") $ pathInfo request

matchURL :: Request -> EndpointDefinition -> Bool
matchURL request (method, url) = requestMethod request == method && (isJust $ matchRegex url (getRequestURL request))

isUnauthorizedEndpoint :: Request -> [EndpointDefinition] -> Bool
isUnauthorizedEndpoint request unauthorizedEndpoints =
  if requestMethod request == methodOptions
    then True
    else or $ matchURL request <$> unauthorizedEndpoints

getTokenFromHeader :: Request -> Maybe T.Text
getTokenFromHeader request =
  case lookup (mk . BS.pack $ authorizationHeaderName) (requestHeaders request) of
    Just headerValue -> separateToken . decodeUtf8 $ headerValue
    Nothing -> Nothing

authMiddleware :: AppConfig -> [EndpointDefinition] -> Middleware
authMiddleware appConfig unauthorizedEndpoints app request sendResponse =
  if isUnauthorizedEndpoint request unauthorizedEndpoints
    then app request sendResponse
    else authorize
  where
    jwtSecret :: String
    jwtSecret = appConfig ^. jwt ^. secret
    jwtVersion :: Integer
    jwtVersion = appConfig ^. jwt ^. version
    authorize :: IO ResponseReceived
    authorize =
      case getTokenFromHeader request of
        Just token -> do
          now <- liftIO getCurrentTime
          case verifyToken token jwtSecret jwtVersion now of
            Nothing -> app request sendResponse
            Just error -> sendResponse . unauthorizedL $ error
        Nothing -> sendResponse . unauthorizedL $ _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN
