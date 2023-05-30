module WizardLib.Public.Service.UserToken.UserTokenUtil where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import qualified Crypto.PubKey.RSA as RSA
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U
import GHC.Records
import qualified Jose.Jwa as JWA
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT

import Shared.Common.Model.Error.Error
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsJM ()
import WizardLib.Public.Localization.Messages.Public

createSignedJwtToken
  :: ( MonadReader s m
     , HasField "serverConfig'" s sc
     , HasField "general" sc scGeneral
     , HasField "rsaPrivateKey" scGeneral RSA.PrivateKey
     , MonadIO m
     , MonadError AppError m
     , ToJSON content
     )
  => content
  -> m JWT.Jwt
createSignedJwtToken content = do
  context <- ask
  let serverConfig = context.serverConfig'
  let jwk = JWK.RsaPrivateJwk serverConfig.general.rsaPrivateKey Nothing Nothing Nothing
  eJwt <- liftIO $ JWT.encode [jwk] (JWT.JwsEncoding JWA.RS256) (JWT.Claims . BSL.toStrict . encode $ content)
  case eJwt of
    Right jwt -> return jwt
    Left error -> throwError . UserError . _ERROR_SERVICE_OPENID__UNABLE_TO_ENCODE_JWT_TOKEN $ show error

getUserUuidFromToken :: String -> Maybe String
getUserUuidFromToken token = do
  let eClaims = JWT.decodeClaims (BS.pack token) :: Either JWT.JwtError (JWT.JwtHeader, UserTokenClaimsDTO)
  case eClaims of
    Right (_, claims) -> Just . U.toString $ claims.userUuid
    _ -> Nothing

getTokenUuidFromToken :: String -> Maybe U.UUID
getTokenUuidFromToken token = do
  let eClaims = JWT.decodeClaims (BS.pack token) :: Either JWT.JwtError (JWT.JwtHeader, UserTokenClaimsDTO)
  case eClaims of
    Right (_, claims) -> Just claims.tokenUuid
    _ -> Nothing
