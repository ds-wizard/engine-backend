module Registry.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Servant (throwError)

import LensesConfig
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Localization.Messages.Internal
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Util.Logger
import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Internal
import Shared.Util.Token
import Shared.Util.Uuid

runInUnauthService :: AppContextM a -> BaseContextM a
runInUnauthService = runIn Nothing

runInAuthService :: Organization -> AppContextM a -> BaseContextM a
runInAuthService org = runIn (Just org)

runIn :: Maybe Organization -> AppContextM a -> BaseContextM a
runIn mOrganization function = do
  traceUuid <- liftIO generateUuid
  serverConfig <- asks _baseContextServerConfig
  localization <- asks _baseContextLocalization
  buildInfoConfig <- asks _baseContextBuildInfoConfig
  dbPool <- asks _baseContextPool
  let appContext =
        AppContext
          { _appContextServerConfig = serverConfig
          , _appContextLocalization = localization
          , _appContextBuildInfoConfig = buildInfoConfig
          , _appContextPool = dbPool
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentOrganization = mOrganization
          }
  let loggingLevel = serverConfig ^. logging . level
  eResult <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  organization <- getCurrentOrganization tokenHeader
  callback (runInAuthService organization)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) callback = do
  org <- getCurrentOrganization token
  callback (runInAuthService org)
getAuthServiceExecutor Nothing _ =
  throwError =<< (sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentOrganization :: String -> BaseContextM Organization
getCurrentOrganization tokenHeader = do
  orgToken <- getCurrentOrgToken tokenHeader
  mOrg <- runInUnauthService (findOrganizationByToken' orgToken)
  case mOrg of
    Just org -> return org
    Nothing -> throwError =<< (sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION)

getCurrentOrgToken :: String -> BaseContextM String
getCurrentOrgToken tokenHeader = do
  let orgTokenMaybe = separateToken tokenHeader
  case orgTokenMaybe of
    Just orgToken -> return orgToken
    Nothing -> throwError =<< (sendErrorDTO . UnauthorizedErrorDTO $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)
