module Wizard.Service.UserToken.UserTokenUtil where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Read
import qualified Web.JWT as JWT

signToken :: String -> JWT.JWTClaimsSet -> String
signToken secret cs =
  let key = JWT.hmacSecret $ T.pack secret
   in T.unpack $ JWT.encodeSigned key mempty cs

getUserUuidFromToken :: String -> Maybe String
getUserUuidFromToken token = do
  (String value) <- getValueFromToken (T.pack token) "userUuid"
  Just . T.unpack $ value

getTokenUuidFromToken :: String -> Maybe String
getTokenUuidFromToken token = do
  (String value) <- getValueFromToken (T.pack token) "tokenUuid"
  Just . T.unpack $ value

getJWTVersionFromToken :: T.Text -> Maybe Integer
getJWTVersionFromToken token = do
  (String value) <- getValueFromToken token "version"
  case decimal value of
    Right (int, _) -> Just int
    Left _ -> Nothing

getExpirationFromToken :: T.Text -> Maybe JWT.NumericDate
getExpirationFromToken token = do
  decodedToken <- JWT.decode token
  let cs = JWT.claims decodedToken
  JWT.exp cs

getValueFromToken :: T.Text -> T.Text -> Maybe Value
getValueFromToken token paramName = do
  decodedToken <- JWT.decode token
  let cs = JWT.claims decodedToken
  let payload = JWT.unClaimsMap . JWT.unregisteredClaims $ cs
  M.lookup paramName payload
