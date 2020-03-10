module Registry.Api.Handler.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Data.Aeson (encode)
import qualified Data.UUID as U
import Servant
  ( Header
  , Headers
  , ServerError(..)
  , addHeader
  , err400
  , err401
  , err401
  , err403
  , err404
  , err500
  , errBody
  , errHeaders
  , throwError
  )

import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Localization.Messages.Internal
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Util.Logger (logError)
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Localization.Locale
import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Shared.Util.Token
import Shared.Util.Uuid

runInUnauthService :: AppContextM a -> BaseContextM a
runInUnauthService function = do
  traceUuid <- liftIO generateUuid
  appConfig <- asks _baseContextAppConfig
  localization <- asks _baseContextLocalization
  buildInfoConfig <- asks _baseContextBuildInfoConfig
  dbPool <- asks _baseContextPool
  let appContext =
        AppContext
          { _appContextApplicationConfig = appConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentOrganization = Nothing
          }
  eResult <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> do
      dto <- sendError error
      throwError dto

runInAuthService :: Organization -> AppContextM a -> BaseContextM a
runInAuthService organization function = do
  traceUuid <- liftIO generateUuid
  appConfig <- asks _baseContextAppConfig
  localization <- asks _baseContextLocalization
  buildInfoConfig <- asks _baseContextBuildInfoConfig
  dbPool <- asks _baseContextPool
  let appContext =
        AppContext
          { _appContextApplicationConfig = appConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentOrganization = Just organization
          }
  eResult <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> do
      dto <- sendError error
      throwError dto

getMaybeAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  organization <- getCurrentOrganization tokenHeader
  callback (runInAuthService organization)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) callback = do
  org <- getCurrentOrganization token
  callback (runInAuthService org)
getAuthServiceExecutor Nothing _ = do
  dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
  throwError dto

getCurrentOrganization :: String -> BaseContextM Organization
getCurrentOrganization tokenHeader = do
  orgToken <- getCurrentOrgToken tokenHeader
  mOrg <- runInUnauthService (findOrganizationByToken' orgToken)
  case mOrg of
    Just org -> return org
    Nothing -> do
      dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION
      throwError dto

getCurrentOrgToken :: String -> BaseContextM String
getCurrentOrgToken tokenHeader = do
  let orgTokenMaybe = separateToken tokenHeader
  case orgTokenMaybe of
    Just orgToken -> return orgToken
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

sendErrorDTO :: ErrorDTO -> BaseContextM ServerError
sendErrorDTO AcceptedErrorDTO =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
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
