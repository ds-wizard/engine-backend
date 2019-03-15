module Service.Token.TokenService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Read
import Data.Time
import qualified Data.Vector as V
import qualified Web.JWT as JWT

import Api.Resource.Token.TokenCreateDTO
import Api.Resource.Token.TokenDTO
import Database.DAO.User.UserDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.Token.Token
import Model.User.User
import Service.Token.TokenMapper
import Util.Date

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
      eitherUser <- findUserByEmail (toLower <$> tokenCreateDto ^. email)
      case eitherUser of
        Right user -> callback user
        Left (NotExistsError _) ->
          return . Left $ createErrorWithErrorMessage _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
        Left error -> return . Left $ error
    -- ------------------------------------------------------------
    checkIsUserActive user callback =
      if user ^. active
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

createToken :: User -> UTCTime -> String -> Integer -> Integer -> Token
createToken user now jwtSecret jwtVersion jwtExpirationInDays =
  let uUuid = toJSON (user ^. uuid) :: Value
      permissionValues = fromString <$> (user ^. permissions)
      uPermissions = Array (V.fromList permissionValues) :: Value
      timeDelta = realToFrac $ jwtExpirationInDays * nominalDayInSeconds
      mExpiration = toNumericDate (addUTCTime timeDelta now)
      cs =
        JWT.JWTClaimsSet
        { iss = Nothing
        , sub = Nothing
        , aud = Nothing
        , exp = mExpiration
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
    createPayload :: Value -> Value -> Value -> JWT.ClaimsMap
    createPayload uUuid uPermissions jwtVersion =
      JWT.ClaimsMap { unClaimsMap = M.insert "version" jwtVersion $ M.insert "permissions" uPermissions $ M.insert "userUuid" uUuid $ M.empty }

signToken :: String -> JWT.JWTClaimsSet -> Token
signToken jwtSecret cs =
  let key = JWT.hmacSecret $ T.pack jwtSecret
  in T.unpack $ JWT.encodeSigned key cs

verifyToken :: T.Text -> String -> Integer -> UTCTime -> Maybe String
verifyToken jwtToken jwtSecret currentJwtVersion now =
  verifySignature $ \token -> verifyJwtVersion $ \() -> verifyExpiration token $ \() -> Nothing
  where
    verifySignature callback =
      case JWT.decodeAndVerifySignature (JWT.hmacSecret (T.pack jwtSecret)) jwtToken of
        Just token -> callback token
        Nothing -> Just _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
    verifyJwtVersion callback =
      case getJWTVersionFromToken jwtToken of
        Just tokenJwtVersion ->
          if tokenJwtVersion == currentJwtVersion
            then callback ()
            else Just _ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION
        Nothing -> Just _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_VERSION
    verifyExpiration token callback =
      case getExpirationFromToken jwtToken of
        Just expiration ->
          case toNumericDate now of
            Just nowInNumericDateFormat ->
              if nowInNumericDateFormat < expiration
                then callback ()
                else Just _ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED
            Nothing -> Just _ERROR_SERVICE_TOKEN__UNKNOWN_TECHNICAL_DIFFICULTIES
        Nothing -> Just _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_EXPIRATION

getUserUuidFromToken :: T.Text -> Maybe T.Text
getUserUuidFromToken token = do
  (String value) <- getValueFromToken token "userUuid"
  Just value

getPermissionsFromToken :: T.Text -> Maybe [Permission]
getPermissionsFromToken token = do
  (Array value) <- getValueFromToken token "permissions"
  let values = V.toList value
  let permissionValues = fmap (\(String x) -> T.unpack x) values
  Just permissionValues

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
