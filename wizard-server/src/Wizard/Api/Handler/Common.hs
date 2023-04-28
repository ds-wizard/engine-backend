module Wizard.Api.Handler.Common where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Constant.App
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Token
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Constant.UserToken
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Service.UserToken.UserTokenValidation
import Wizard.Util.Context
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO
import WizardLib.Public.Model.User.UserToken

runInUnauthService :: Maybe String -> TransactionState -> AppContextM a -> BaseContextM a
runInUnauthService mServerUrl transactionState function = do
  app <- getCurrentApp mServerUrl
  runIn app Nothing transactionState function

runInAuthService :: Maybe String -> UserDTO -> TransactionState -> AppContextM a -> BaseContextM a
runInAuthService mServerUrl user transactionState function = do
  app <- getCurrentApp mServerUrl
  runIn app (Just user) transactionState function

runIn :: App -> Maybe UserDTO -> TransactionState -> AppContextM a -> BaseContextM a
runIn app mUser transactionState function = do
  if app.enabled
    then do
      baseContext <- ask
      let loggingLevel = baseContext.serverConfig.logging.level
      eResult <-
        liftIO $ appContextFromBaseContext app.uuid mUser transactionState baseContext $ \appContext -> do
          liftIO $ runMonads (runAppContextM function) appContext
      case eResult of
        Right result -> return result
        Left error -> throwError =<< sendError error
    else do
      throwError =<< sendError LockedError

runWithSystemUser :: AppContextM a -> BaseContextM a
runWithSystemUser function = do
  baseContext <- ask
  let loggingLevel = baseContext.serverConfig.logging.level
  eResult <-
    liftIO $ appContextFromBaseContext defaultAppUuid (Just . toDTO $ userSystem) NoTransaction baseContext $ \appContext -> do
      liftIO $ runMonads (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor
  :: Maybe String
  -> Maybe String
  -> ((TransactionState -> AppContextM a -> BaseContextM a) -> BaseContextM b)
  -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) mServerUrl callback =
  getAuthServiceExecutor (Just tokenHeader) mServerUrl callback
getMaybeAuthServiceExecutor Nothing mServerUrl callback = callback (runInUnauthService mServerUrl)

getAuthServiceExecutor
  :: Maybe String
  -> Maybe String
  -> ((TransactionState -> AppContextM a -> BaseContextM a) -> BaseContextM b)
  -> BaseContextM b
getAuthServiceExecutor (Just token) mServerUrl callback = do
  userTokenClaims <- validateJwtToken token
  isTokenExistsInDb userTokenClaims mServerUrl
  user <- getCurrentUser userTokenClaims mServerUrl
  if user.active
    then callback (runInAuthService mServerUrl user)
    else throwError =<< (sendError . UnauthorizedError $ _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED)
getAuthServiceExecutor Nothing _ _ =
  throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentUser :: UserTokenClaimsDTO -> Maybe String -> BaseContextM UserDTO
getCurrentUser userTokenClaims mServerUrl = do
  let userUuid = userTokenClaims.userUuid
  runInUnauthService mServerUrl NoTransaction $ catchError (getUserById userUuid) (handleError userUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__USER_ABSENCE . U.toString $ userUuid)
    handleError userUuid error = throwError error

validateJwtToken :: String -> BaseContextM UserTokenClaimsDTO
validateJwtToken tokenHeader = do
  baseContext <- ask
  let serverConfig = baseContext.serverConfig
  now <- liftIO getCurrentTime
  case separateToken tokenHeader of
    Just jwtToken -> do
      eUserTokenClaims <- decodeAndValidateJwtToken jwtToken serverConfig.general.rsaPrivateKey userTokenVersion now
      case eUserTokenClaims of
        Right userTokenClaims -> return userTokenClaims
        Left error -> throwError =<< sendError (UnauthorizedError error)
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

isTokenExistsInDb :: UserTokenClaimsDTO -> Maybe String -> BaseContextM UserToken
isTokenExistsInDb userTokenClaims mServerUrl = do
  let tokenUuid = userTokenClaims.tokenUuid
  runInUnauthService mServerUrl NoTransaction $ catchError (findUserTokenByUuid tokenUuid) (handleError tokenUuid)
  where
    handleError tokenUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__TOKEN_ABSENCE . U.toString $ tokenUuid)
    handleError tokenUuid error = throwError error

getCurrentApp :: Maybe String -> BaseContextM App
getCurrentApp mServerUrl = do
  baseContext <- ask
  let serverConfig = baseContext.serverConfig
  if serverConfig.cloud.enabled
    then case mServerUrl of
      Nothing -> runWithSystemUser . throwError $ UnauthorizedError (_ERROR_VALIDATION__APP_ABSENCE "not-provided")
      Just serverUrl -> do
        runWithSystemUser $ catchError (findAppByServerDomain serverUrl) (handleError serverUrl)
    else
      runWithSystemUser $
        catchError (findAppByUuid defaultAppUuid) (handleError (U.toString defaultAppUuid))
  where
    handleError host (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__APP_ABSENCE host)
    handleError host error = throwError error

isAdmin :: AppContextM Bool
isAdmin = do
  mUser <- asks currentUser
  case mUser of
    Just user -> return $ user.uRole == _USER_ROLE_ADMIN
    Nothing -> return False
