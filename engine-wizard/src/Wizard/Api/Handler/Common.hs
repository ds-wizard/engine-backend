module Wizard.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, asks, liftIO)
import qualified Data.UUID as U

import LensesConfig
import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.App
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Token
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Service.Token.TokenService
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Util.Context

runInUnauthService :: Maybe String -> AppContextM a -> BaseContextM a
runInUnauthService mServerUrl function = do
  app <- getCurrentApp mServerUrl
  runIn app Nothing function

runInAuthService :: Maybe String -> UserDTO -> AppContextM a -> BaseContextM a
runInAuthService mServerUrl user function = do
  app <- getCurrentApp mServerUrl
  runIn app (Just user) function

runIn :: App -> Maybe UserDTO -> AppContextM a -> BaseContextM a
runIn app mUser function = do
  if app ^. enabled
    then do
      baseContext <- ask
      let loggingLevel = baseContext ^. serverConfig . logging . level
      eResult <-
        liftIO $ appContextFromBaseContext (app ^. uuid) mUser baseContext $ \appContext -> do
          liftIO $ runMonads (runAppContextM function) appContext
      case eResult of
        Right result -> return result
        Left error -> throwError =<< sendError error
    else do
      throwError =<< sendError LockedError

runWithSystemUser :: AppContextM a -> BaseContextM a
runWithSystemUser function = do
  baseContext <- ask
  let loggingLevel = baseContext ^. serverConfig . logging . level
  eResult <-
    liftIO $ appContextFromBaseContext defaultAppUuid (Just . toDTO $ userSystem) baseContext $ \appContext -> do
      liftIO $ runMonads (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor ::
     Maybe String -> Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) mServerUrl callback = do
  user <- getCurrentUser tokenHeader mServerUrl
  callback (runInAuthService mServerUrl user)
getMaybeAuthServiceExecutor Nothing mServerUrl callback = callback (runInUnauthService mServerUrl)

getAuthServiceExecutor ::
     Maybe String -> Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) mServerUrl callback = do
  user <- getCurrentUser token mServerUrl
  callback (runInAuthService mServerUrl user)
getAuthServiceExecutor Nothing _ _ =
  throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentUser :: String -> Maybe String -> BaseContextM UserDTO
getCurrentUser tokenHeader mServerUrl = do
  userUuid <- getCurrentUserUuid tokenHeader
  runInUnauthService mServerUrl $ catchError (getUserById userUuid) (handleError userUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__USER_ABSENCE userUuid)
    handleError userUuid error = throwError error

getCurrentApp :: Maybe String -> BaseContextM App
getCurrentApp mServerUrl = do
  serverConfig <- asks _baseContextServerConfig
  if serverConfig ^. cloud . enabled
    then case mServerUrl of
           Nothing -> runWithSystemUser . throwError $ UnauthorizedError (_ERROR_VALIDATION__APP_ABSENCE "not-provided")
           Just serverUrl -> do
             runWithSystemUser $ catchError (findAppByServerDomain serverUrl) (handleError serverUrl)
    else runWithSystemUser $
         catchError (findAppById . U.toString $ defaultAppUuid) (handleError (U.toString defaultAppUuid))
  where
    handleError host (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__APP_ABSENCE host)
    handleError host error = throwError error

getCurrentUserUuid :: String -> BaseContextM String
getCurrentUserUuid tokenHeader = do
  let userUuidMaybe = separateToken tokenHeader >>= getUserUuidFromToken
  case userUuidMaybe of
    Just userUuid -> return userUuid
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

isAdmin :: AppContextM Bool
isAdmin = do
  mUser <- asks _appContextCurrentUser
  case mUser of
    Just user -> return $ user ^. role == _USER_ROLE_ADMIN
    Nothing -> return False
