module Wizard.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Data.Aeson
import qualified Data.UUID as U
import Servant
  ( Header
  , Headers
  , ServantErr(..)
  , addHeader
  , err400
  , err401
  , err401
  , err403
  , err404
  , err500
  , errBody
  , errHeaders
  )

import LensesConfig
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Localization.Locale
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Token
import Shared.Util.Uuid
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Token.TokenService
import Wizard.Service.User.UserService
import Wizard.Util.Logger (logError)

runInUnauthService :: AppContextM a -> BaseContextM a
runInUnauthService function = do
  traceUuid <- liftIO generateUuid
  appConfig <- asks _baseContextAppConfig
  localization <- asks _baseContextLocalization
  buildInfoConfig <- asks _baseContextBuildInfoConfig
  dbPool <- asks _baseContextPool
  msgChannel <- asks _baseContextMsgChannel
  httpClientManager <- asks _baseContextHttpClientManager
  let appContext =
        AppContext
          { _appContextApplicationConfig = appConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextMsgChannel = msgChannel
          , _appContextHttpClientManager = httpClientManager
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentUser = Nothing
          }
  eResult <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> do
      dto <- sendError error
      throwError dto

runInAuthService :: UserDTO -> AppContextM a -> BaseContextM a
runInAuthService user function = do
  traceUuid <- liftIO generateUuid
  appConfig <- asks _baseContextAppConfig
  localization <- asks _baseContextLocalization
  buildInfoConfig <- asks _baseContextBuildInfoConfig
  dbPool <- asks _baseContextPool
  msgChannel <- asks _baseContextMsgChannel
  httpClientManager <- asks _baseContextHttpClientManager
  let appContext =
        AppContext
          { _appContextApplicationConfig = appConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextMsgChannel = msgChannel
          , _appContextHttpClientManager = httpClientManager
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentUser = Just user
          }
  eResult <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
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

sendError :: AppError -> BaseContextM ServantErr
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
  logError errorMessage
  return $ err500 {errBody = encode $ GeneralServerErrorDTO errorMessage, errHeaders = [contentTypeHeaderJSON]}

sendErrorDTO :: ErrorDTO -> BaseContextM ServantErr
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
  logError message
  return $ err500 {errBody = encode $ GeneralServerErrorDTO message, errHeaders = [contentTypeHeaderJSON]}

checkPermission mTokenHeader perm = do
  let mUserPerms = mTokenHeader >>= separateToken >>= getPermissionsFromToken
      forbidden = throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ perm)
  case mUserPerms of
    Just userPerms -> unless (perm `Prelude.elem` userPerms) forbidden
    Nothing -> forbidden

isAdmin :: AppContextM Bool
isAdmin = do
  mUser <- asks _appContextCurrentUser
  case mUser of
    Just user -> return $ user ^. role == "ADMIN"
    Nothing -> return False

checkServiceToken :: Maybe String -> AppContextM ()
checkServiceToken mTokenHeader = do
  appConfig <- asks _appContextApplicationConfig
  let mToken = mTokenHeader >>= separateToken >>= validateServiceToken appConfig
  case mToken of
    Just _ -> return ()
    Nothing -> throwError . UnauthorizedError $ _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN
  where
    validateServiceToken appConfig token =
      if token == appConfig ^. general . serviceToken
        then Just token
        else Nothing
