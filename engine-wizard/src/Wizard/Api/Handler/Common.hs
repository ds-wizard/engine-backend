module Wizard.Api.Handler.Common where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, asks, liftIO)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import Servant.Multipart (Input (..))

import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.App
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Util.Token
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Service.UserToken.UserTokenUtil
import Wizard.Service.UserToken.UserTokenValidation
import Wizard.Util.Context

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
  isValidJwtToken token
  isTokenExistsInDb token mServerUrl
  user <- getCurrentUser token mServerUrl
  if user.active
    then callback (runInAuthService mServerUrl user)
    else throwError =<< (sendError . UnauthorizedError $ _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED)
getAuthServiceExecutor Nothing _ _ =
  throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentUser :: String -> Maybe String -> BaseContextM UserDTO
getCurrentUser tokenHeader mServerUrl = do
  userUuid <- getCurrentUserUuid tokenHeader
  runInUnauthService mServerUrl NoTransaction $ catchError (getUserById userUuid) (handleError userUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__USER_ABSENCE userUuid)
    handleError userUuid error = throwError error

isValidJwtToken :: String -> BaseContextM ()
isValidJwtToken tokenHeader = do
  baseContext <- ask
  let serverConfig = baseContext.serverConfig
  now <- liftIO getCurrentTime
  case separateToken tokenHeader of
    Just jwtToken ->
      case validateJwtToken jwtToken serverConfig.general.secret serverConfig.jwt.version now of
        Nothing -> return ()
        Just error -> throwError =<< sendError (UnauthorizedError error)
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

isTokenExistsInDb :: String -> Maybe String -> BaseContextM UserToken
isTokenExistsInDb tokenHeader mServerUrl = do
  tokenUuid <- getCurrentTokenUuid tokenHeader
  runInUnauthService mServerUrl NoTransaction $ catchError (findUserTokenById tokenUuid) (handleError tokenUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__TOKEN_ABSENCE userUuid)
    handleError userUuid error = throwError error

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

getCurrentTokenUuid :: String -> BaseContextM String
getCurrentTokenUuid tokenHeader = do
  let tokenUuidMaybe = separateToken tokenHeader >>= getTokenUuidFromToken
  case tokenUuidMaybe of
    Just tokenUuid -> return tokenUuid
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

isAdmin :: AppContextM Bool
isAdmin = do
  mUser <- asks currentUser
  case mUser of
    Just user -> return $ user.uRole == _USER_ROLE_ADMIN
    Nothing -> return False

-- --------------------------------
-- MULTIPART
-- --------------------------------
getTextFromInput :: T.Text -> [Input] -> AppContextM T.Text
getTextFromInput name inputs = do
  case getMaybeTextFromInput name inputs of
    Just text -> return text
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__FIELD_ABSENCE (T.unpack name)

getMaybeTextFromInput :: T.Text -> [Input] -> Maybe T.Text
getMaybeTextFromInput name = fmap iValue . L.find (\i -> iName i == name)

getStringFromInput :: T.Text -> [Input] -> AppContextM String
getStringFromInput name inputs = fmap T.unpack $ getTextFromInput name inputs

getMaybeStringFromInput :: T.Text -> [Input] -> Maybe String
getMaybeStringFromInput name inputs = fmap T.unpack $ getMaybeTextFromInput name inputs
