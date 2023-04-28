module Wizard.Service.UserToken.Login.LoginService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Jose.Jwt as JWT

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Number
import Shared.Common.Util.Token
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Mail.Mailer
import qualified Wizard.Service.User.UserMapper as UserMapper
import Wizard.Service.UserToken.Login.LoginMapper
import Wizard.Service.UserToken.Login.LoginValidation
import Wizard.Service.UserToken.UserTokenMapper
import Wizard.Service.UserToken.UserTokenUtil
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Model.User.UserToken

createLoginTokenFromCredentials :: LoginDTO -> Maybe String -> AppContextM UserTokenDTO
createLoginTokenFromCredentials reqDto mUserAgent =
  runInTransaction $ do
    mUser <- findUserByEmail' (fmap toLower reqDto.email)
    case mUser of
      Just user -> do
        validate reqDto user
        appConfig <- getAppConfig
        now <- liftIO getCurrentTime
        case (appConfig.authentication.internal.twoFactorAuth.enabled, reqDto.code) of
          (False, _) -> do
            updateUserLastVisitedAtByUuid user.uuid now
            createLoginToken user mUserAgent Nothing
          (True, Nothing) -> do
            deleteActionKeyByUserId user.uuid
            let length = appConfig.authentication.internal.twoFactorAuth.codeLength
            let min = 10 ^ (length - 1)
            let max = (10 ^ length) - 1
            code <- liftIO $ generateIntInRange min max
            createActionKeyWithHash user.uuid TwoFactorAuthActionKey user.appUuid (show code)
            sendTwoFactorAuthMail (UserMapper.toDTO user) (show code)
            return CodeRequiredDTO
          (True, Just code) -> do
            validateCode user code appConfig
            deleteActionKeyByUserIdAndHash user.uuid (show code)
            updateUserLastVisitedAtByUuid user.uuid now
            createLoginToken user mUserAgent Nothing
      Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

createLoginToken :: User -> Maybe String -> Maybe String -> AppContextM UserTokenDTO
createLoginToken user mUserAgent mSessionState =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let claims = toUserTokenClaims user uuid now serverConfig.jwt
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
