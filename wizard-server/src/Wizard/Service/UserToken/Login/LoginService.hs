module Wizard.Service.UserToken.Login.LoginService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import qualified Jose.Jwt as JWT

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Number
import Shared.Common.Util.Token
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Tenant.Config.ConfigService
import qualified Wizard.Service.User.UserMapper as UserMapper
import Wizard.Service.UserToken.Login.LoginMapper
import Wizard.Service.UserToken.Login.LoginValidation
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper
import WizardLib.Public.Service.UserToken.UserTokenUtil

createLoginTokenFromCredentials :: LoginDTO -> Maybe String -> AppContextM UserTokenDTO
createLoginTokenFromCredentials reqDto mUserAgent =
  runInTransaction $ do
    mUser <- findUserByEmail' (fmap toLower reqDto.email)
    case mUser of
      Just user -> do
        validate reqDto user
        tcAuthentication <- getCurrentTenantConfigAuthentication
        now <- liftIO getCurrentTime
        case (tcAuthentication.internal.twoFactorAuth.enabled, reqDto.code) of
          (False, _) -> do
            updateUserLastVisitedAtByUuid user.uuid now
            createLoginToken user mUserAgent Nothing
          (True, Nothing) -> do
            deleteActionKeyByIdentity (U.toString user.uuid)
            let length = tcAuthentication.internal.twoFactorAuth.codeLength
            let min = 10 ^ (length - 1)
            let max = (10 ^ length) - 1
            code <- liftIO $ generateIntInRange min max
            createActionKeyWithHash user.uuid TwoFactorAuthActionKey user.tenantUuid (show code)
            sendTwoFactorAuthMail (UserMapper.toDTO user) (show code)
            return CodeRequiredDTO
          (True, Just code) -> do
            validateCode user code tcAuthentication
            deleteActionKeyByIdentityAndHash (U.toString user.uuid) (show code)
            updateUserLastVisitedAtByUuid user.uuid now
            createLoginToken user mUserAgent Nothing
      Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

createLoginToken :: User -> Maybe String -> Maybe String -> AppContextM UserTokenDTO
createLoginToken user mUserAgent mSessionState =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let claims = toUserTokenClaims user.uuid uuid user.tenantUuid now serverConfig.jwt.expiration
    (JWT.Jwt jwtToken) <- createSignedJwtToken claims
    let userToken = fromLoginDTO uuid user serverConfig.jwt.expiration serverConfig.general.secret mUserAgent mSessionState now (BS.unpack jwtToken)
    insertUserToken userToken
    return . toDTO $ userToken

deleteLoginTokenByValue :: Maybe String -> AppContextM ()
deleteLoginTokenByValue mTokenHeader = do
  case fmap separateToken mTokenHeader of
    Just (Just tokenValue) -> do
      userTokens <- findUserTokensByValue tokenValue
      traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens
    _ -> return ()

deleteLoginTokenBySessionState :: Maybe String -> AppContextM ()
deleteLoginTokenBySessionState mSessionState = do
  case mSessionState of
    Just sessionState -> do
      userTokens <- findUserTokensBySessionState sessionState
      traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens
    Nothing -> return ()
