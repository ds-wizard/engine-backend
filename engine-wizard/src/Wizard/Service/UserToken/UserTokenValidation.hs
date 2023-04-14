module Wizard.Service.UserToken.UserTokenValidation where

import qualified Data.Text as T
import Data.Time
import qualified Web.JWT as JWT

import Shared.Model.Localization.LocaleRecord
import Wizard.Localization.Messages.Public
import Wizard.Service.UserToken.UserTokenUtil
import Wizard.Util.Date

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
