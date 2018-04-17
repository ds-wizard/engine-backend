module Api.Middleware.AuthMiddleware where

import Control.Lens ((^.))
import Data.ByteString (ByteString, pack)
import Data.CaseInsensitive (mk)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Method (methodOptions)
import Network.Wai
       (Application, Middleware, Request, Response, ResponseReceived,
        pathInfo, requestHeaders, requestMethod, responseLBS)
import Prelude hiding (exp)
import Text.Regex
import qualified Web.JWT as JWT

import Api.Handler.Common
import Common.Types
import Common.Utils
import LensesConfig
import Model.Config.DSWConfig

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
    authorize :: IO ResponseReceived
    authorize =
      case getTokenFromHeader request of
        Just token -> verifyToken token
        Nothing -> sendResponse unauthorizedL
    verifyToken :: T.Text -> IO ResponseReceived
    verifyToken jwtToken =
      case JWT.decodeAndVerifySignature (JWT.secret (T.pack jwtSecret)) jwtToken of
        Just token -> app request sendResponse
        Nothing -> sendResponse unauthorizedL
