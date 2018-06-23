module Service.Token.TokenService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
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
import Utils.Date

getToken :: TokenCreateDTO -> AppContextM (Either AppError TokenDTO)
getToken tokenCreateDto =
  getUser $ \user ->
    checkIsUserActive user $ \() ->
      authenticateUser user $ \() -> do
        (jwtSecret, jwtVersion, jwtExpirationInDays) <- getJwtConfig
        now <- liftIO getCurrentTime
        return . Right . toDTO $ createToken user now jwtSecret jwtVersion jwtExpirationInDays
  where
    getUser callback = do
      eitherUser <- findUserByEmail (tokenCreateDto ^. email)
      case eitherUser of
        Right user -> callback user
        Left (NotExistsError _) ->
          return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
        Left error -> return . Left $ error
    -- ------------------------------------------------------------
    checkIsUserActive user callback =
      if user ^. isActive
        then callback ()
        else return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
    -- ------------------------------------------------------------
    authenticateUser user callback = do
      let incomingPassword = BS.pack (tokenCreateDto ^. password)
      let passwordHashFromDB = BS.pack (user ^. passwordHash)
      if verifyPassword incomingPassword passwordHashFromDB
        then callback ()
        else return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
    -- ------------------------------------------------------------
    getJwtConfig = do
      dswConfig <- asks _appContextConfig
      let config = dswConfig ^. jwtConfig
      return (config ^. secret, config ^. version, config ^. expiration)

createToken :: User -> UTCTime -> JWTSecret -> Integer -> Integer -> Token
createToken user now jwtSecret jwtVersion jwtExpirationInDays =
  let uUuid = toJSON (user ^. uuid) :: Value
      permissionValues = fromString <$> (user ^. permissions)
      uPermissions = Array (V.fromList permissionValues) :: Value
      timeDelta = realToFrac $ jwtExpirationInDays * nominalDay
      expiration = JWT.numericDate $ diffUTCTime (addUTCTime timeDelta now) day0
      cs =
        JWT.JWTClaimsSet
        { iss = Nothing
        , sub = Nothing
        , aud = Nothing
        , exp = expiration
        , nbf = Nothing
        , iat = Nothing
        , jti = Nothing
        , unregisteredClaims = createPayload uUuid uPermissions (fromInteger $ jwtVersion)
        }
  in signToken jwtSecret cs
  where
    fromInteger :: Integer -> Value
    fromInteger = fromString . show
    fromString :: String -> Value
    fromString = String . T.pack
    createPayload uUuid uPermissions jwtVersion = M.insert "version" jwtVersion $ M.insert "permissions" uPermissions $ M.insert "userUuid" uUuid $ M.empty

signToken :: JWTSecret -> JWT.JWTClaimsSet -> Token
signToken jwtSecret cs =
  let key = JWT.secret $ T.pack jwtSecret
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
