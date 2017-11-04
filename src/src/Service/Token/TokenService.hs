module Service.Token.TokenService where

import Control.Lens ((^.))
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import qualified Web.JWT as JWT

import Api.Resources.Token.TokenCreateDTO
import Api.Resources.Token.TokenDTO
import Common.Types
import Common.Utils
import Context
import DSPConfig
import Data.Aeson
import Database.DAO.User.UserDAO
import Model.User.User
import Service.Token.TokenMapper

getToken :: Context -> DSPConfig -> TokenCreateDTO -> IO (Maybe TokenDTO)
getToken context dspConfig tokenCreateDto = do
  let secret = dspConfig ^. dspcfgJwtConfig ^. acjwtSecret
  maybeUser <- findUserByEmail context (tokenCreateDto ^. tcdtoEmail)
  case maybeUser of
    Just user -> do
      let incomingPassword = BS.pack (tokenCreateDto ^. tcdtoPassword)
      let passwordHashFromDB = BS.pack (user ^. uPasswordHash)
      if verifyPassword incomingPassword passwordHashFromDB
        then return . Just . toDTO $ createToken user secret
        else return Nothing
    Nothing -> return Nothing

createToken :: User -> JWTSecret -> Token
createToken user jwtSecret =
  let permissionValues = fmap (String . T.pack) (user ^. uPermissions)
      permissions = Array (V.fromList permissionValues) :: Value
      userUuid = toJSON (user ^. uUuid) :: Value
      payload = M.insert "userUuid" userUuid M.empty
      payload2 = M.insert "permissions" permissions payload
      cs =
        JWT.JWTClaimsSet
        { iss = Nothing
        , sub = Nothing
        , aud = Nothing
        , exp = Nothing
        , nbf = Nothing
        , iat = Nothing
        , jti = Nothing
        , unregisteredClaims = payload2
        }
      key = JWT.secret $ T.pack jwtSecret
  in T.unpack $ JWT.encodeSigned JWT.HS256 key cs

getUserUuidFromToken :: Context -> Maybe T.Text -> Maybe T.Text
getUserUuidFromToken context maybeTokenHeaderValue =
  case maybeTokenHeaderValue of
    Just tokenHeaderValue -> do
      decodedToken <- separateToken tokenHeaderValue >>= JWT.decode
      let cs = JWT.claims decodedToken
      let payload = JWT.unregisteredClaims cs
      (String userUuid) <- M.lookup "userUuid" payload
      Just userUuid
    _ -> Nothing
