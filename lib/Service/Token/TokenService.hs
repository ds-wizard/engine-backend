module Service.Token.TokenService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Web.JWT as JWT

import Api.Resource.Token.TokenCreateDTO
import Api.Resource.Token.TokenDTO
import Common.Error
import Common.Localization
import Common.Types
import Common.Utils
import Database.DAO.User.UserDAO
import LensesConfig
import Model.Context.AppContext
import Model.User.User
import Service.Token.TokenMapper

getToken :: TokenCreateDTO -> AppContextM (Either AppError TokenDTO)
getToken tokenCreateDto = do
  dswConfig <- asks _appContextConfig
  let tokenSecret = dswConfig ^. jwtConfig ^. secret
  eitherUser <- findUserByEmail (tokenCreateDto ^. email)
  case eitherUser of
    Right user ->
      if user ^. isActive
        then do
          let incomingPassword = BS.pack (tokenCreateDto ^. password)
          let passwordHashFromDB = BS.pack (user ^. passwordHash)
          if verifyPassword incomingPassword passwordHashFromDB
            then return . Right . toDTO $ createToken user tokenSecret
            else return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
        else return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
    Left (NotExistsError _) ->
      return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
    Left error -> return . Left $ error

createToken :: User -> JWTSecret -> Token
createToken user jwtSecret =
  let permissionValues = fmap (String . T.pack) (user ^. permissions)
      uPermissions = Array (V.fromList permissionValues) :: Value
      userUuid = toJSON (user ^. uuid) :: Value
      payload = M.insert "userUuid" userUuid M.empty
      payload2 = M.insert "permissions" uPermissions payload
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

getUserUuidFromToken :: Maybe T.Text -> Maybe T.Text
getUserUuidFromToken maybeTokenHeaderValue = do
  (String value) <- getValueFromToken maybeTokenHeaderValue "userUuid"
  Just value

getPermissionsFromToken :: Maybe T.Text -> Maybe [Permission]
getPermissionsFromToken maybeTokenHeaderValue = do
  (Array value) <- getValueFromToken maybeTokenHeaderValue "permissions"
  let values = V.toList value
  let permissionValues = fmap (\(String x) -> T.unpack x) values
  Just permissionValues

getValueFromToken :: Maybe T.Text -> T.Text -> Maybe Value
getValueFromToken maybeTokenHeaderValue paramName =
  case maybeTokenHeaderValue of
    Just tokenHeaderValue -> do
      decodedToken <- separateToken tokenHeaderValue >>= JWT.decode
      let cs = JWT.claims decodedToken
      let payload = JWT.unregisteredClaims cs
      M.lookup paramName payload
    _ -> Nothing
