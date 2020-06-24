module Wizard.Service.Token.TokenService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Read
import Data.Time
import qualified Data.UUID as U
import qualified Web.JWT as JWT

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Token.Token
import Wizard.Service.Token.TokenMapper
import Wizard.Util.Date

generateTokenFromCredentials :: TokenCreateDTO -> AppContextM TokenDTO
generateTokenFromCredentials tokenCreateDto = do
  user <- getUser
  checkIsUserActive user
  authenticateUser user
  serverConfig <- asks _appContextServerConfig
  now <- liftIO getCurrentTime
  return . toDTO $ createToken user now (serverConfig ^. jwt) (serverConfig ^. general . secret)
  where
    getUser = do
      mUser <- findUserByEmail' (toLower <$> tokenCreateDto ^. email)
      case mUser of
        Just user -> return user
        Nothing -> throwError $ UnauthorizedError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
    -- ------------------------------------------------------------
    checkIsUserActive user =
      if user ^. active
        then return ()
        else throwError $ UnauthorizedError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
    -- ------------------------------------------------------------
    authenticateUser user = do
      let incomingPassword = BS.pack (tokenCreateDto ^. password)
      let passwordHashFromDB = BS.pack (user ^. passwordHash)
      if verifyPassword incomingPassword passwordHashFromDB
        then return ()
        else throwError $ UnauthorizedError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

generateTokenFromUser :: UserDTO -> AppContextM TokenDTO
generateTokenFromUser user = do
  serverConfig <- asks _appContextServerConfig
  now <- liftIO getCurrentTime
  return . toDTO $ createToken user now (serverConfig ^. jwt) (serverConfig ^. general . secret)

createToken :: (HasUuid user U.UUID) => user -> UTCTime -> ServerConfigJwt -> String -> Token
createToken user now config secret =
  let uUuid = toJSON (user ^. uuid) :: Value
      timeDelta = realToFrac $ (config ^. expiration) * nominalDayInSeconds
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
          , unregisteredClaims = createPayload uUuid (fromInteger $ config ^. version)
          }
   in signToken secret cs
  where
    fromInteger :: Integer -> Value
    fromInteger = fromString . show
    fromString :: String -> Value
    fromString = String . T.pack
    createPayload :: Value -> Value -> JWT.ClaimsMap
    createPayload uUuid jwtVersion =
      JWT.ClaimsMap {unClaimsMap = M.insert "version" jwtVersion $ M.insert "userUuid" uUuid M.empty}

signToken :: String -> JWT.JWTClaimsSet -> Token
signToken secret cs =
  let key = JWT.hmacSecret $ T.pack secret
   in T.unpack $ JWT.encodeSigned key mempty cs

verifyToken :: T.Text -> String -> Integer -> UTCTime -> Maybe String
verifyToken jwtToken secret currentJwtVersion now =
  verifySignature $ \token -> verifyJwtVersion $ \() -> verifyExpiration token $ \() -> Nothing
  where
    verifySignature callback =
      case JWT.decodeAndVerifySignature (JWT.hmacSecret (T.pack secret)) jwtToken of
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

getUserUuidFromToken :: String -> Maybe String
getUserUuidFromToken token = do
  (String value) <- getValueFromToken (T.pack token) "userUuid"
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
