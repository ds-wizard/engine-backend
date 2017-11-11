module Api.Middleware.Auth where

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
       (Middleware, Application, ResponseReceived, Request, Response,
        requestHeaders, requestMethod, responseLBS, pathInfo)
import Prelude hiding (exp)
import Text.Regex
import Web.JWT

import Api.Handler.Common
import Common.Types
import Common.Utils
import DSPConfig

authorizationHeaderName :: ByteString
authorizationHeaderName = "Authorization"

getRequestURL :: Request -> String
getRequestURL request = T.unpack . (T.intercalate "/") $ pathInfo request

matchURL :: String -> Regex -> Bool
matchURL requestURL unauthorizedEndpoint =
  isJust $ matchRegex unauthorizedEndpoint requestURL

isUnauthorizedEndpoint :: Request -> [Regex] -> Bool
isUnauthorizedEndpoint request unauthorizedEndpoints =
  if requestMethod request == methodOptions
    then True
    else or $ fmap (matchURL . getRequestURL $ request) unauthorizedEndpoints

getTokenFromHeader :: Request -> Maybe T.Text
getTokenFromHeader request =
  case lookup (mk authorizationHeaderName) (requestHeaders request) of
    Just headerValue -> separateToken . decodeUtf8 $ headerValue
    Nothing -> Nothing

authMiddleware :: DSPConfig -> [Regex] -> Middleware
authMiddleware dspConfig unauthorizedEndpoints app request sendResponse = do
  if isUnauthorizedEndpoint request unauthorizedEndpoints
    then app request sendResponse
    else authorize
  where
    jwtSecret :: JWTSecret
    jwtSecret = dspConfig ^. dspcfgJwtConfig ^. acjwtSecret
    authorize :: IO ResponseReceived
    authorize =
      case getTokenFromHeader request of
        Just token -> verifyToken $ token
        Nothing -> sendResponse unauthorizedL
    verifyToken :: T.Text -> IO ResponseReceived
    verifyToken jwtToken =
      case decodeAndVerifySignature (secret (T.pack jwtSecret)) jwtToken of
        Just token -> app request sendResponse
        Nothing -> sendResponse unauthorizedL
