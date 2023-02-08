module Wizard.Service.UserToken.UserTokenValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import Data.Time
import qualified Web.JWT as JWT

import Shared.Model.Error.Error
import Shared.Model.Localization.LocaleRecord
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.User.UserUtil
import Wizard.Service.UserToken.UserTokenUtil
import Wizard.Util.Date

validate :: UserTokenCreateDTO -> User -> AppContextM ()
validate reqDto user = do
  validateIsUserActive user
  validateUserPassword reqDto user

validateIsUserActive :: User -> AppContextM ()
validateIsUserActive user =
  if user.active
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED

validateUserPassword :: UserTokenCreateDTO -> User -> AppContextM ()
validateUserPassword reqDto user =
  if verifyPassword reqDto.password user.passwordHash
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

validateCode :: User -> Int -> AppConfig -> AppContextM ()
validateCode user code appConfig = do
  actionKey <- findActionKeyByUserIdAndHash user.uuid (show code)
  let timeDelta = realToFrac . toInteger $ appConfig.authentication.internal.twoFactorAuth.expiration
  now <- liftIO getCurrentTime
  when (addUTCTime timeDelta actionKey.createdAt < now) (throwError $ UserError _ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED)

validateJwtToken :: String -> String -> Integer -> UTCTime -> Maybe LocaleRecord
validateJwtToken jwtToken secret currentJwtVersion now =
  verifySignature jwtToken secret $ \() ->
    verifyJwtVersion jwtToken currentJwtVersion $ \() -> verifyExpiration jwtToken now $ \() -> Nothing
  where
    verifyJwtVersion jwtToken currentJwtVersion callback =
      case getJWTVersionFromToken (T.pack jwtToken) of
        Just tokenJwtVersion ->
          if tokenJwtVersion == currentJwtVersion
            then callback ()
            else Just _ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION
        Nothing -> Just _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_VERSION
    verifySignature jwtToken secret callback =
      case JWT.decodeAndVerifySignature (JWT.toVerify . JWT.hmacSecret . T.pack $ secret) (T.pack jwtToken) of
        Just _ -> callback ()
        Nothing -> Just _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
    verifyExpiration jwtToken now callback =
      case getExpirationFromToken (T.pack jwtToken) of
        Just expiration ->
          case toNumericDate now of
            Just nowInNumericDateFormat ->
              if nowInNumericDateFormat < expiration
                then callback ()
                else Just _ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED
            Nothing -> Just _ERROR_SERVICE_TOKEN__UNKNOWN_TECHNICAL_DIFFICULTIES
        Nothing -> Just _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_EXPIRATION
