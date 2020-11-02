module Wizard.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, asks, liftIO)
import qualified Data.UUID as U
import Servant (ServerError(..))

import Data.Time
import LensesConfig
import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Shared.Util.Token
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Service.Token.TokenService
import Wizard.Service.User.UserService
import Wizard.Util.Context

runInUnauthService :: AppContextM a -> BaseContextM a
runInUnauthService = runIn Nothing

runInAuthService :: UserDTO -> AppContextM a -> BaseContextM a
runInAuthService user = runIn (Just user)

runInServiceAuthService :: AppContextM a -> BaseContextM a
runInServiceAuthService function = do
  serverConfig <- asks _baseContextServerConfig
  now <- liftIO getCurrentTime
  let user = createServiceUser serverConfig now
  runIn (Just user) function

runIn :: Maybe UserDTO -> AppContextM a -> BaseContextM a
runIn mUser function = do
  baseContext <- ask
  appContext <- liftIO $ appContextFromBaseContext mUser baseContext
  let loggingLevel = baseContext ^. serverConfig . logging . level
  eResult <- liftIO $ runMonads (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  user <- getCurrentUser tokenHeader
  callback (runInAuthService user)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) callback = do
  user <- getCurrentUser token
  callback (runInAuthService user)
getAuthServiceExecutor Nothing _ =
  throwError =<< (sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getServiceTokenOrAuthServiceExecutor ::
     Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getServiceTokenOrAuthServiceExecutor mTokenHeader callback =
  (do checkServiceToken' mTokenHeader
      callback runInServiceAuthService) `catchError`
  handleError
  where
    handleError ServerError {errHTTPCode = 401} = getAuthServiceExecutor mTokenHeader callback
    handleError rest = throwError rest

getServiceTokenOrMaybeAuthServiceExecutor ::
     Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getServiceTokenOrMaybeAuthServiceExecutor mTokenHeader callback =
  (do checkServiceToken' mTokenHeader
      callback runInServiceAuthService) `catchError`
  handleError
  where
    handleError ServerError {errHTTPCode = 401} = getMaybeAuthServiceExecutor mTokenHeader callback
    handleError rest = throwError rest

getCurrentUser :: String -> BaseContextM UserDTO
getCurrentUser tokenHeader = do
  userUuid <- getCurrentUserUuid tokenHeader
  runInUnauthService $ catchError (getUserByIdDto userUuid) (handleError userUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__USER_ABSENCE userUuid)
    handleError userUuid error = throwError error

getCurrentUserUuid :: String -> BaseContextM String
getCurrentUserUuid tokenHeader = do
  let userUuidMaybe = separateToken tokenHeader >>= getUserUuidFromToken
  case userUuidMaybe of
    Just userUuid -> return userUuid
    Nothing -> throwError =<< (sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

isAdmin :: AppContextM Bool
isAdmin = do
  mUser <- asks _appContextCurrentUser
  case mUser of
    Just user -> return $ user ^. role == _USER_ROLE_ADMIN
    Nothing -> return False

checkServiceToken :: Maybe String -> AppContextM ()
checkServiceToken mTokenHeader = do
  serverConfig <- asks _appContextServerConfig
  let mToken = mTokenHeader >>= separateToken >>= validateServiceToken serverConfig
  case mToken of
    Just _ -> return ()
    Nothing -> throwError . UnauthorizedError $ _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN

checkServiceToken' :: Maybe String -> BaseContextM ()
checkServiceToken' mTokenHeader = do
  serverConfig <- asks _baseContextServerConfig
  let mToken = mTokenHeader >>= separateToken >>= validateServiceToken serverConfig
  case mToken of
    Just _ -> return ()
    Nothing ->
      throwError =<<
      (sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN')

validateServiceToken :: ServerConfig -> String -> Maybe String
validateServiceToken serverConfig token =
  if token == serverConfig ^. general . serviceToken
    then Just token
    else Nothing

createServiceUser :: ServerConfig -> UTCTime -> UserDTO
createServiceUser serverConfig now =
  UserDTO
    { _userDTOUuid = U.nil
    , _userDTOFirstName = "Service"
    , _userDTOLastName = "User"
    , _userDTOEmail = "service@user.com"
    , _userDTOAffiliation = Nothing
    , _userDTOSources = [_USER_SOURCE_INTERNAL]
    , _userDTORole = _USER_ROLE_ADMIN
    , _userDTOPermissions = serverConfig ^. roles . admin
    , _userDTOActive = True
    , _userDTOImageUrl = Nothing
    , _userDTOGroups = []
    , _userDTOCreatedAt = Just now
    , _userDTOUpdatedAt = Just now
    }
