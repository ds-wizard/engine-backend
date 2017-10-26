module Api.Middleware.Auth where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe (isJust)
import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Types as H
import Network.Wai
       (Middleware, Application, ResponseReceived, Request, Response,
        requestHeaders, responseLBS, pathInfo)
import Prelude hiding (exp)
import Text.Regex
import Web.JWT

import Api.Handler.Common
import DSPConfig
import Common.Types

authorizationHeaderName :: ByteString
authorizationHeaderName = "x-dsp-auth-token"

getRequestURL :: Request -> String
getRequestURL request = T.unpack . T.concat $ pathInfo request

matchURL :: String -> Regex -> Bool
matchURL requestURL unauthorizedEndpoint =
  isJust $ matchRegex unauthorizedEndpoint requestURL

isUnauthorizedEndpoint :: String -> [Regex] -> Bool
isUnauthorizedEndpoint requestURL unauthorizedEndpoints =
  or $ fmap (matchURL requestURL) unauthorizedEndpoints

getTokenFromHeader :: Request -> Maybe ByteString
getTokenFromHeader request =
  lookup (mk authorizationHeaderName) (requestHeaders request)

authMiddleware :: DSPConfig -> [Regex] -> Middleware
authMiddleware dspConfig unauthorizedEndpoints app request sendResponse =
  if isUnauthorizedEndpoint (getRequestURL request) unauthorizedEndpoints
    then app request sendResponse
    else authorize
  where
    jwtSecret :: JWTSecret
    jwtSecret = dspConfig ^. dspcfgJwtConfig ^. acjwtSecret
    authorize :: IO ResponseReceived
    authorize =
      case getTokenFromHeader request of
        Just token -> verifyToken . decodeUtf8 $ token
        Nothing -> sendResponse unauthorizedL
    verifyToken :: T.Text -> IO ResponseReceived
    verifyToken jwtToken =
      case decodeAndVerifySignature (secret (T.pack jwtSecret)) jwtToken of
        Just token -> app request sendResponse
        Nothing -> sendResponse unauthorizedL
