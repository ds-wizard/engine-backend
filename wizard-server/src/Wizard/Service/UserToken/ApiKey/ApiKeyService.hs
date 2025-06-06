module Wizard.Service.UserToken.ApiKey.ApiKeyService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Time
import qualified Jose.Jwt as JWT

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Service.Mail.Mailer
import Wizard.Service.UserToken.ApiKey.ApiKeyMapper
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper
import WizardLib.Public.Service.UserToken.UserTokenUtil

createApiKey :: ApiKeyCreateDTO -> Maybe String -> AppContextM UserTokenDTO
createApiKey reqDto mUserAgent =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    uuid <- liftIO generateUuid
    userDto <- getCurrentUser
    user <- findUserByUuid userDto.uuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let claims = toUserTokenClaimsWithExpiration user.uuid uuid user.tenantUuid now reqDto.expiresAt
    (JWT.Jwt jwtToken) <- createSignedJwtToken claims
    let userToken = fromApiKeyDTO reqDto uuid user.uuid serverConfig.general.secret mUserAgent tenantUuid now (BS.unpack jwtToken)
    insertUserToken userToken
    sendApiKeyCreatedMail userDto userToken
    return . toDTO $ userToken

expireApiKeys :: AppContextM ()
expireApiKeys = do
  userTokens <- findApiUserTokensWithCloseExpiration
  traverse_
    ( \userToken -> do
        user <- findUserByUuidAndTenantUuidSystem userToken.userUuid userToken.tenantUuid
        sendApiKeyExpirationMail user userToken
    )
    userTokens
