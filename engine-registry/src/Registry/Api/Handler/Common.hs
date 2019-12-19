module Registry.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader (asks, lift, liftIO, runReaderT)
import Data.Aeson ((.=), eitherDecode, encode, object)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as U
import Network.HTTP.Types (hContentType, notFound404)
import Network.HTTP.Types.Method (methodOptions)
import Network.HTTP.Types.Status (badRequest400, forbidden403, internalServerError500, ok200, unauthorized401)
import Network.Wai
import Web.Scotty.Trans
  ( ActionT
  , ScottyError
  , addHeader
  , body
  , header
  , json
  , liftAndCatchIO
  , params
  , raw
  , request
  , showError
  , status
  )

import Registry.Constant.Component
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.LensesConfig hiding (requestMethod)
import Registry.Localization
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Util.Logger
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (authorizationHeaderName, xTraceUuidHeaderName)
import Shared.Localization.Locale
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Token
import Shared.Util.Uuid

type Endpoint = ActionT LT.Text BaseContextM ()

runInUnauthService function = do
  traceUuid <- liftIO generateUuid
  addHeader (LT.pack xTraceUuidHeaderName) (LT.pack . U.toString $ traceUuid)
  appConfig <- lift $ asks _baseContextAppConfig
  localization <- lift $ asks _baseContextLocalization
  buildInfoConfig <- lift $ asks _baseContextBuildInfoConfig
  dbPool <- lift $ asks _baseContextPool
  let appContext =
        AppContext
          { _appContextApplicationConfig = appConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentOrganization = Nothing
          }
  liftAndCatchIO $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext

runInAuthService organization function = do
  traceUuid <- liftIO generateUuid
  addHeader (LT.pack xTraceUuidHeaderName) (LT.pack . U.toString $ traceUuid)
  appConfig <- lift $ asks _baseContextAppConfig
  localization <- lift $ asks _baseContextLocalization
  buildInfoConfig <- lift $ asks _baseContextBuildInfoConfig
  dbPool <- lift $ asks _baseContextPool
  let appContext =
        AppContext
          { _appContextApplicationConfig = appConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentOrganization = Just organization
          }
  liftAndCatchIO $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext

getAuthServiceExecutor callback = getCurrentOrganization $ \organization -> callback $ runInAuthService organization

getMaybeAuthServiceExecutor callback =
  isLogged $ \orgIsLogged ->
    if orgIsLogged
      then getCurrentOrganization $ \organization -> callback $ runInAuthService organization
      else callback runInUnauthService

getReqDto callback = do
  reqBody <- body
  let eitherReqDto = eitherDecode reqBody
  case eitherReqDto of
    Right reqDto -> callback reqDto
    Left error -> do
      lift . logWarn $ msg _CMP_API (show error)
      sendError $ UserError _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

getCurrentOrgToken callback = do
  tokenHeader <- header (LT.pack authorizationHeaderName)
  let orgTokenMaybe = tokenHeader >>= (\token -> Just . LT.toStrict $ token) >>= separateToken :: Maybe T.Text
  case orgTokenMaybe of
    Just orgToken -> callback (T.unpack orgToken)
    Nothing -> unauthorizedA _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN

getCurrentOrganization callback =
  getCurrentOrgToken $ \orgToken -> do
    eitherOrganization <- runInUnauthService $ findOrganizationByToken orgToken
    case eitherOrganization of
      Right organization -> callback organization
      Left error -> unauthorizedA _ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION

getQueryParam paramName = do
  reqParams <- params
  let mValue = lookup paramName reqParams
  case mValue of
    Just value -> return . Just . LT.toStrict $ value
    Nothing -> return Nothing

getListOfQueryParamsIfPresent :: [LT.Text] -> ActionT LT.Text BaseContextM [(T.Text, T.Text)]
getListOfQueryParamsIfPresent = Prelude.foldr go (return [])
  where
    go name monadAcc = do
      value <- extractQueryParam name
      acc <- monadAcc
      return $ maybeToList value ++ acc
    extractQueryParam name = do
      mValue <- getQueryParam name
      case mValue of
        Just value -> return $ Just (LT.toStrict name, value)
        Nothing -> return Nothing

getListOfHeaders :: [String] -> ActionT LT.Text BaseContextM [(String, String)]
getListOfHeaders = Prelude.foldr go (return [])
  where
    go name monadAcc = do
      value <- extractHeader name
      acc <- monadAcc
      return $ maybeToList value ++ acc
    extractHeader name = do
      mValue <- header (LT.pack name)
      case mValue of
        Just value -> return $ Just (name, LT.unpack value)
        Nothing -> return Nothing

isLogged callback = do
  tokenHeader <- header (LT.pack authorizationHeaderName)
  callback . isJust $ tokenHeader

isAdmin callback =
  isLogged $ \orgIsLogged ->
    if orgIsLogged
      then getCurrentOrganization $ \organization -> callback $ organization ^. role == AdminRole
      else callback False

sendError :: AppError -> Endpoint
sendError (ValidationError formErrorRecords fieldErrorRecords) = do
  ls <- lift . asks $ _baseContextLocalization
  let formErrors = fmap (locale ls) formErrorRecords
  let localeTuple = \(k, v) -> (k, locale ls v)
  let fieldErrors = fmap localeTuple fieldErrorRecords
  status badRequest400
  json $ ValidationErrorDTO formErrors fieldErrors
sendError (UserError localeRecord) = do
  ls <- lift . asks $ _baseContextLocalization
  let message = locale ls localeRecord
  status badRequest400
  json $ UserErrorDTO message
sendError (UnauthorizedError localeRecord) = do
  ls <- lift . asks $ _baseContextLocalization
  let message = locale ls localeRecord
  status unauthorized401
  json $ UnauthorizedErrorDTO message
sendError (ForbiddenError localeRecord) = do
  ls <- lift . asks $ _baseContextLocalization
  let message = locale ls localeRecord
  status forbidden403
  json $ ForbiddenErrorDTO message
sendError (NotExistsError localeRecord) = do
  ls <- lift . asks $ _baseContextLocalization
  let message = locale ls localeRecord
  status notFound404
  json $ NotExistsErrorDTO message
sendError (GeneralServerError errorMessage) = do
  lift $ logError errorMessage
  status internalServerError500
  json $ GeneralServerErrorDTO errorMessage

sendFile :: String -> BSL.ByteString -> Endpoint
sendFile filename body = do
  let cdHeader = "attachment;filename=" ++ filename
  addHeader "Content-Disposition" (LT.pack cdHeader)
  addHeader "Content-Type" (LT.pack "application/octet-stream")
  raw body

unauthorizedA :: String -> Endpoint
unauthorizedA message = do
  status unauthorized401
  json $ object ["status" .= 401, "error" .= "Unauthorized", "message" .= message]

unauthorizedL :: String -> Response
unauthorizedL message =
  responseLBS unauthorized401 [(hContentType, "application/json; charset=utf-8")] $
  encode (object ["status" .= 401, "error" .= "Unauthorized", "message" .= message])

forbidden :: Endpoint
forbidden = do
  status forbidden403
  json $ object ["status" .= 403, "error" .= "Forbidden"]

notFoundA :: Endpoint
notFoundA = do
  request <- request
  if requestMethod request == methodOptions
    then status ok200
    else do
      lift . logInfo $ msg _CMP_API "Request does not match any route"
      status notFound404
      json $ object ["status" .= 404, "error" .= "Not Found"]

internalServerErrorA :: (ScottyError e, Monad m, MonadLogger m) => e -> ActionT e m ()
internalServerErrorA e = do
  let message = LT.unpack . showError $ e
  lift . logError $ message
  status internalServerError500
  json . GeneralServerErrorDTO $ message
