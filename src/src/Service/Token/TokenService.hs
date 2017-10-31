module Service.Token.TokenService where

import Control.Lens ((^.))
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Web.JWT

import Api.Resources.Token.TokenCreateDTO
import Api.Resources.Token.TokenDTO
import Common.Types
import Context
import DSPConfig
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
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
