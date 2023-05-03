module WizardLib.Public.Service.UserToken.UserTokenValidation where

import Control.Monad.Reader (MonadIO, liftIO)
import qualified Crypto.PubKey.RSA as RSA
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import Data.Time.Clock.POSIX
import qualified Jose.Jwa as JWA
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT

import Shared.Common.Model.Localization.LocaleRecord
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsJM ()
import WizardLib.Public.Localization.Messages.Public

decodeAndValidateJwtToken :: MonadIO m => String -> RSA.PrivateKey -> Integer -> UTCTime -> m (Either LocaleRecord UserTokenClaimsDTO)
decodeAndValidateJwtToken token privateKey currentJwtVersion now = do
  eDecoded <- liftIO $ JWT.decode [JWK.RsaPrivateJwk privateKey Nothing Nothing Nothing] (Just $ JWT.JwsEncoding JWA.RS256) (BS.pack token)
  case eDecoded of
    Right (JWT.Jws (_, claimsS)) ->
      case eitherDecode . BSL.fromStrict $ claimsS :: Either String UserTokenClaimsDTO of
        Right claims -> do
          if claims.version == currentJwtVersion
            then do
              if JWT.IntDate (utcTimeToPOSIXSeconds now) < claims.exp
                then return . Right $ claims
                else return . Left $ _ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED
            else return . Left $ _ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION
        Left error -> return . Left $ _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
    _ -> return . Left $ _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
