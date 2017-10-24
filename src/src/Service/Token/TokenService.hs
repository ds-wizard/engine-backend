module Service.Token.TokenService where

import Control.Lens ((^.))
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Web.JWT

import Api.Resources.Token.TokenCreateDTO
import Api.Resources.Token.TokenDTO
import Common.JWT
import Common.Types
import Context
import Database.DAO.UserDAO
import Database.Entity.User
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Aeson

import Service.Token.TokenMapper

getToken :: Context -> TokenCreateDTO -> IO (Maybe TokenDTO)
getToken context tokenCreateDto
    -- secret <- getSecret
 = do
  maybeUser <- findUserByEmail context (tokenCreateDto ^. tcdtoEmail)
  case maybeUser of
    Just user -> do
      let incomingPassword = BS.pack (tokenCreateDto ^. tcdtoPassword)
      let passwordHashFromDB = BS.pack (user ^. uPasswordHash)
      if verifyPassword incomingPassword passwordHashFromDB
        then return . Just . toDTO $ createToken user "secret-key"
        else return Nothing
    Nothing -> return Nothing

createToken :: User -> JWTSecret -> Token
createToken user jwtSecret =
  let permissionValues = fmap (String . T.pack) (user ^. uPermissions)
      val = Array (V.fromList permissionValues) :: Value
      payload = M.insert "roles" val M.empty
      cs =
        JWTClaimsSet
        { iss = Nothing
        , sub = Nothing
        , aud = Nothing
        , exp = Nothing
        , nbf = Nothing
        , iat = Nothing
        , jti = Nothing
        , unregisteredClaims = payload
        }
      key = secret $ T.pack jwtSecret
  in T.unpack $ encodeSigned HS256 key cs
