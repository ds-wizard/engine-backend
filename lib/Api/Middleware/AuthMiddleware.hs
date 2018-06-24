module Api.Middleware.AuthMiddleware where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Method (methodOptions)
import Network.Wai
       (Middleware, Request, ResponseReceived, pathInfo, requestHeaders,
        requestMethod)
import Prelude hiding (exp)
import Text.Regex

import Api.Handler.Common
import Common.Localization
import Common.Types
import Common.Utils
import LensesConfig
import Model.Config.DSWConfig
import Service.Token.TokenService

type EndpointDefinition = (H.Method, Regex)

authorizationHeaderName :: ByteString
authorizationHeaderName = "Authorization"

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
  case lookup (mk authorizationHeaderName) (requestHeaders request) of
    Just headerValue -> separateToken . decodeUtf8 $ headerValue
    Nothing -> Nothing

authMiddleware :: DSWConfig -> [EndpointDefinition] -> Middleware
authMiddleware dswConfig unauthorizedEndpoints app request sendResponse =
  if isUnauthorizedEndpoint request unauthorizedEndpoints
    then app request sendResponse
    else authorize
  where
    jwtSecret :: JWTSecret
    jwtSecret = dswConfig ^. jwtConfig ^. secret
    jwtVersion :: Integer
    jwtVersion = dswConfig ^. jwtConfig ^. version
    authorize :: IO ResponseReceived
    authorize =
      case getTokenFromHeader request of
        Just token -> do
          now <- liftIO getCurrentTime
          case verifyToken token jwtSecret jwtVersion now of
            Nothing -> app request sendResponse
            Just error -> sendResponse . unauthorizedL $ error
        Nothing -> sendResponse . unauthorizedL $ _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN
