module Registry.Api.Handler.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Data.Pool
import Servant (throwError)

import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Config.ServerConfig
import qualified Registry.Model.Context.AppContext as AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Util.Logger
import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Token
import Shared.Common.Util.Uuid

runInUnauthService :: TransactionState -> AppContext.AppContextM a -> BaseContextM a
runInUnauthService = runIn Nothing

runInAuthService :: Organization -> TransactionState -> AppContext.AppContextM a -> BaseContextM a
runInAuthService org = runIn (Just org)

runIn :: Maybe Organization -> TransactionState -> AppContext.AppContextM a -> BaseContextM a
runIn mOrganization transactionState function = do
  traceUuid <- liftIO generateUuid
  serverConf <- asks serverConfig
  buildInfoConfig <- asks buildInfoConfig
  dbPool <- asks dbPool
  s3Client <- asks s3Client
  let appContext =
        AppContext.AppContext
          { serverConfig = serverConf
          , buildInfoConfig = buildInfoConfig
          , dbPool = dbPool
          , dbConnection = Nothing
          , s3Client = s3Client
          , traceUuid = traceUuid
          , currentOrganization = mOrganization
          }
  let loggingLevel = serverConf.logging.level
  eResult <-
    case transactionState of
      Transactional -> do
        liftIO $ withResource dbPool $ \dbConn -> do
          let appContextWithConn = appContext {AppContext.dbConnection = Just dbConn} :: AppContext.AppContext
          liftIO $ runExceptT $ runLogging loggingLevel $ runReaderT (AppContext.runAppContextM function) appContextWithConn
      NoTransaction -> liftIO $ runExceptT $ runLogging loggingLevel $ runReaderT (AppContext.runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor
  :: Maybe String -> ((TransactionState -> AppContext.AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  organization <- getCurrentOrganization tokenHeader
  callback (runInAuthService organization)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor
  :: Maybe String -> ((TransactionState -> AppContext.AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) callback = do
  org <- getCurrentOrganization token
  callback (runInAuthService org)
getAuthServiceExecutor Nothing _ =
  throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentOrganization :: String -> BaseContextM Organization
getCurrentOrganization tokenHeader = do
  orgToken <- getCurrentOrgToken tokenHeader
  mOrg <- runInUnauthService NoTransaction (findOrganizationByToken' orgToken)
  case mOrg of
    Just org -> return org
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION)

getCurrentOrgToken :: String -> BaseContextM String
getCurrentOrgToken tokenHeader = do
  let orgTokenMaybe = separateToken tokenHeader
  case orgTokenMaybe of
    Just orgToken -> return orgToken
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)
