module Wizard.Service.UserToken.System.SystemService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Constant.UserToken
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Integration.Http.Admin.Runner
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Service.UserToken.System.SystemMapper
import Wizard.Service.UserToken.System.SystemValidation
import Wizard.Service.UserToken.UserTokenMapper
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsJM ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Service.UserToken.UserTokenUtil

createSystemToken :: String -> Maybe String -> AppContextM UserTokenDTO
createSystemToken token mUserAgent =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    (JWK.JwkSet keys) <- retrieveJwtPublicKeys
    eUserTokenClaims <- decodeAndValidateJwtToken token keys userTokenVersion now
    case eUserTokenClaims of
      Right userTokenClaims -> do
        serverConfig <- asks serverConfig
        user <- findUserByUuid userTokenClaims.userUuid
        uuid <- liftIO generateUuid
        updateUserLastVisitedAtByUuid user.uuid now
        let claims = toUserTokenClaims user uuid now serverConfig.jwt
        (JWT.Jwt jwtToken) <- createSignedJwtToken claims
        let userToken = fromSystemDTO uuid user serverConfig.jwt.expiration serverConfig.general.secret mUserAgent Nothing now (BS.unpack jwtToken)
        insertUserToken userToken
        return . toDTO $ userToken
      Left error -> throwError . UnauthorizedError $ error
