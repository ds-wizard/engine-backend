module Wizard.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U
import Network.HTTP.Types.Status
import Servant
  ( Header
  , Headers
  , ServerError(..)
  , addHeader
  , err302
  , err400
  , err401
  , err401
  , err403
  , err404
  , err500
  , errBody
  , errHeaders
  )

import Data.Time
import LensesConfig
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Localization.Locale
import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Shared.Util.Token
import Shared.Util.Uuid
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
import Wizard.Util.Logger

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
  traceUuid <- liftIO generateUuid
  serverConfig <- asks _baseContextServerConfig
  localization <- asks _baseContextLocalization
  buildInfoConfig <- asks _baseContextBuildInfoConfig
  dbPool <- asks _baseContextPool
  msgChannel <- asks _baseContextMsgChannel
  httpClientManager <- asks _baseContextHttpClientManager
  registryClient <- asks _baseContextRegistryClient
  cache <- asks _baseContextCache
  now <- liftIO getCurrentTime
  let user = createServiceUser serverConfig now
  let appContext =
        AppContext
          { _appContextServerConfig = serverConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextMsgChannel = msgChannel
          , _appContextHttpClientManager = httpClientManager
          , _appContextRegistryClient = registryClient
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentUser = mUser
          , _appContextCache = cache
          }
  let loggingLevel = serverConfig ^. logging . level
  eResult <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> do
      dto <- sendError error
      throwError dto

getMaybeAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  user <- getCurrentUser tokenHeader
  callback (runInAuthService user)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) callback = do
  user <- getCurrentUser token
  callback (runInAuthService user)
getAuthServiceExecutor Nothing _ = do
  dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
  throwError dto

getServiceTokenOrAuthServiceExecutor ::
     Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getServiceTokenOrAuthServiceExecutor mTokenHeader callback =
  (do checkServiceToken' mTokenHeader
      callback runInServiceAuthService) `catchError`
  handleError
  where
    handleError ServerError {errHTTPCode = 401} = getAuthServiceExecutor mTokenHeader callback
    handleError rest = throwError rest

getCurrentUser :: String -> BaseContextM UserDTO
getCurrentUser tokenHeader = do
  userUuid <- getCurrentUserUuid tokenHeader
  runInUnauthService $ catchError (getUserById userUuid) (handleError userUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__USER_ABSENCE userUuid)
    handleError userUuid error = throwError error

getCurrentUserUuid :: String -> BaseContextM String
getCurrentUserUuid tokenHeader = do
  let userUuidMaybe = separateToken tokenHeader >>= getUserUuidFromToken
  case userUuidMaybe of
    Just userUuid -> return userUuid
    Nothing -> do
      dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      throwError dto

addTraceUuidHeader :: a -> AppContextM (Headers '[ Header "x-trace-uuid" String] a)
addTraceUuidHeader result = do
  traceUuid <- asks _appContextTraceUuid
  return $ addHeader (U.toString traceUuid) result

sendError :: AppError -> BaseContextM ServerError
sendError AcceptedError =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
sendError (FoundError url) =
  return $
  err302 {errBody = encode $ FoundErrorDTO url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendError (ValidationError formErrorRecords fieldErrorRecords) = do
  ls <- asks _baseContextLocalization
  let formErrors = fmap (locale ls) formErrorRecords
  let localeTuple (k, v) = (k, locale ls v)
  let fieldErrors = fmap localeTuple fieldErrorRecords
  return $ err400 {errBody = encode $ ValidationErrorDTO formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendError (UserError localeRecord) = do
  ls <- asks _baseContextLocalization
  let message = locale ls localeRecord
  return $ err400 {errBody = encode $ UserErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (UnauthorizedError localeRecord) = do
  ls <- asks _baseContextLocalization
  let message = locale ls localeRecord
  return $ err401 {errBody = encode $ UnauthorizedErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (ForbiddenError localeRecord) = do
  ls <- asks _baseContextLocalization
  let message = locale ls localeRecord
  return $ err403 {errBody = encode $ ForbiddenErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (NotExistsError localeRecord) = do
  ls <- asks _baseContextLocalization
  let message = locale ls localeRecord
  return $ err404 {errBody = encode $ NotExistsErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (GeneralServerError errorMessage) = do
  logError _CMP_API errorMessage
  return $ err500 {errBody = encode $ GeneralServerErrorDTO errorMessage, errHeaders = [contentTypeHeaderJSON]}
sendError (HttpClientError status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }

sendErrorDTO :: ErrorDTO -> BaseContextM ServerError
sendErrorDTO AcceptedErrorDTO =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
sendErrorDTO (FoundErrorDTO url) =
  return $
  err302 {errBody = encode $ FoundErrorDTO url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendErrorDTO (ValidationErrorDTO formErrors fieldErrors) =
  return $ err400 {errBody = encode $ ValidationErrorDTO formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (UserErrorDTO message) =
  return $ err400 {errBody = encode $ UserErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (UnauthorizedErrorDTO message) =
  return $ err401 {errBody = encode $ UnauthorizedErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (ForbiddenErrorDTO message) =
  return $ err403 {errBody = encode $ ForbiddenErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (NotExistsErrorDTO message) =
  return $ err404 {errBody = encode $ NotExistsErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (GeneralServerErrorDTO message) = do
  logError _CMP_API message
  return $ err500 {errBody = encode $ GeneralServerErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (HttpClientErrorDTO status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }

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
  where
    validateServiceToken serverConfig token =
      if token == serverConfig ^. general . serviceToken
        then Just token
        else Nothing

checkServiceToken' :: Maybe String -> BaseContextM ()
checkServiceToken' mTokenHeader = do
  serverConfig <- asks _baseContextServerConfig
  let mToken = mTokenHeader >>= separateToken >>= validateServiceToken serverConfig
  case mToken of
    Just _ -> return ()
    Nothing -> do
      dto <- sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN'
      throwError dto
  where
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
    , _userDTOCreatedAt = Just now
    , _userDTOUpdatedAt = Just now
    }
