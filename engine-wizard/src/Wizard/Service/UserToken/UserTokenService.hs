module Wizard.Service.UserToken.UserTokenService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Error.Error
import Shared.Util.Number
import Shared.Util.Token
import Shared.Util.Uuid
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenDTO
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
import Wizard.Model.User.UserToken
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Mail.Mailer
import qualified Wizard.Service.User.UserMapper as UserMapper
import Wizard.Service.UserToken.UserTokenMapper
import Wizard.Service.UserToken.UserTokenValidation
import Wizard.Util.Logger

createTokenFromCredentials :: UserTokenCreateDTO -> AppContextM UserTokenDTO
createTokenFromCredentials reqDto =
  runInTransaction $ do
    mUser <- findUserByEmail' (fmap toLower reqDto.email)
    case mUser of
      Just user -> do
        validate reqDto user
        appConfig <- getAppConfig
        now <- liftIO getCurrentTime
        case (appConfig.authentication.internal.twoFactorAuth.enabled, reqDto.code) of
          (False, _) -> do
            updateUserById user {lastVisitedAt = now}
            createToken user Nothing
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
            updateUserById user {lastVisitedAt = now}
            createToken user Nothing
      Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

createToken :: User -> Maybe String -> AppContextM UserTokenDTO
createToken user mSessionState =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    tokenUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let userToken =
          toUserToken user tokenUuid mSessionState now serverConfig.jwt serverConfig.general.secret
    insertUserToken userToken
    return . toDTO $ userToken

deleteTokenByUserUuid :: U.UUID -> AppContextM ()
deleteTokenByUserUuid userUuid = do
  userTokens <- findUserTokensByUserUuid userUuid
  traverse_ (\t -> deleteUserTokenById t.uuid) userTokens

deleteTokenByValue :: Maybe String -> AppContextM ()
deleteTokenByValue mTokenHeader = do
  case fmap separateToken mTokenHeader of
    Just (Just tokenValue) -> do
      userTokens <- findUserTokensByValue tokenValue
      traverse_ (\t -> deleteUserTokenById t.uuid) userTokens
    _ -> return ()

deleteTokenBySessionState :: Maybe String -> AppContextM ()
deleteTokenBySessionState mSessionState = do
  case mSessionState of
    Just sessionState -> do
      userTokens <- findUserTokensBySessionState sessionState
      traverse_ (\t -> deleteUserTokenById t.uuid) userTokens
    Nothing -> return ()

cleanTokens :: AppContextM ()
cleanTokens = do
  serverConfig <- asks serverConfig
  deletedUserTokens <- deleteUserTokensWithExpiration serverConfig.jwt.expiration
  logInfoU _CMP_SERVICE $ f' "Deleted the following %s tokens" [show deletedUserTokens]
