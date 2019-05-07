module Api.Handler.Common where

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
import Network.HTTP.Types.Status
       (badRequest400, forbidden403, internalServerError500, ok200,
        unauthorized401)
import Network.Wai
import Web.Scotty.Trans
       (ActionT, ScottyError, addHeader, body, header, json,
        liftAndCatchIO, params, raw, request, showError, status)

import Api.Resource.Error.ErrorDTO ()
import Constant.Api
       (authorizationHeaderName, xDSWTraceUuidHeaderName)
import Constant.Component
import LensesConfig hiding (requestMethod)
import Localization
import Model.Context.AppContext
import Model.Context.BaseContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Service.Token.TokenService
import Service.User.UserService
import Util.Logger
import Util.Token
import Util.Uuid

type Endpoint = ActionT LT.Text BaseContextM ()

runInUnauthService function = do
  traceUuid <- liftIO generateUuid
  addHeader (LT.pack xDSWTraceUuidHeaderName) (LT.pack . U.toString $ traceUuid)
  dswConfig <- lift $ asks _baseContextConfig
  dbPool <- lift $ asks _baseContextPool
  msgChannel <- lift $ asks _baseContextMsgChannel
  httpClientManager <- lift $ asks _baseContextHttpClientManager
  let appContext =
        AppContext
        { _appContextConfig = dswConfig
        , _appContextPool = dbPool
        , _appContextMsgChannel = msgChannel
        , _appContextHttpClientManager = httpClientManager
        , _appContextTraceUuid = traceUuid
        , _appContextCurrentUser = Nothing
        }
  liftAndCatchIO $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext

runInAuthService user function = do
  traceUuid <- liftIO generateUuid
  addHeader (LT.pack xDSWTraceUuidHeaderName) (LT.pack . U.toString $ traceUuid)
  dswConfig <- lift $ asks _baseContextConfig
  dbPool <- lift $ asks _baseContextPool
  msgChannel <- lift $ asks _baseContextMsgChannel
  httpClientManager <- lift $ asks _baseContextHttpClientManager
  let appContext =
        AppContext
        { _appContextConfig = dswConfig
        , _appContextPool = dbPool
        , _appContextMsgChannel = msgChannel
        , _appContextHttpClientManager = httpClientManager
        , _appContextTraceUuid = traceUuid
        , _appContextCurrentUser = Just user
        }
  liftAndCatchIO $ runStdoutLoggingT $ runReaderT (runAppContextM $ function) appContext

getAuthServiceExecutor callback = getCurrentUser $ \user -> callback $ runInAuthService user

getReqDto callback = do
  reqBody <- body
  let eitherReqDto = eitherDecode reqBody
  case eitherReqDto of
    Right reqDto -> callback reqDto
    Left error -> sendError $ createErrorWithErrorMessage error

getCurrentUserUuid callback = do
  tokenHeader <- header (LT.pack authorizationHeaderName)
  let userUuidMaybe =
        tokenHeader >>= (\token -> Just . LT.toStrict $ token) >>= separateToken >>= getUserUuidFromToken :: Maybe T.Text
  case userUuidMaybe of
    Just userUuid -> callback (T.unpack userUuid)
    Nothing -> unauthorizedA _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN

getCurrentUser callback =
  getCurrentUserUuid $ \userUuid -> do
    eitherUser <- runInUnauthService $ getUserById userUuid
    case eitherUser of
      Right user -> callback user
      Left error -> sendError error

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

checkPermission perm callback = do
  tokenHeader <- header (LT.pack authorizationHeaderName)
  let mUserPerms = tokenHeader >>= (\token -> Just . LT.toStrict $ token) >>= separateToken >>= getPermissionsFromToken
  case mUserPerms of
    Just userPerms ->
      if perm `Prelude.elem` userPerms
        then callback
        else forbidden
    Nothing -> forbidden

checkServiceToken callback = do
  tokenHeader <- header (LT.pack authorizationHeaderName)
  dswConfig <- lift $ asks _baseContextConfig
  let mToken =
        tokenHeader >>= (\token -> Just . LT.toStrict $ token) >>= separateToken >>= validateServiceToken dswConfig
  case mToken of
    Just _ -> callback
    Nothing -> unauthorizedA _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SEVICE_TOKEN
  where
    validateServiceToken dswConfig token = do
      if token == (T.pack $ dswConfig ^. webConfig . serviceToken)
        then Just token
        else Nothing

isLogged callback = do
  tokenHeader <- header (LT.pack authorizationHeaderName)
  callback . isJust $ tokenHeader

isAdmin callback =
  isLogged $ \userIsLogged ->
    if userIsLogged
      then getCurrentUser $ \user -> callback $ user ^. role == "ADMIN"
      else callback False

sendError :: AppError -> Endpoint
sendError (ValidationError errorMessage formErrors fieldErrors) = do
  status badRequest400
  json $ ValidationError errorMessage formErrors fieldErrors
sendError (NotExistsError errorMessage) = do
  status notFound404
  json $ NotExistsError errorMessage
sendError (DatabaseError errorMessage) = do
  lift $ logError errorMessage
  status internalServerError500
  json $ DatabaseError errorMessage
sendError (MigratorError errorMessage) = do
  lift $ logWarn errorMessage
  status badRequest400
  json $ MigratorError errorMessage
sendError (HttpClientError errorMessage) = do
  lift $ logError errorMessage
  status internalServerError500
  json $ HttpClientError errorMessage
sendError (GeneralServerError errorMessage) = do
  lift $ logError errorMessage
  status internalServerError500
  json $ GeneralServerError errorMessage

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
  json . GeneralServerError $ message
