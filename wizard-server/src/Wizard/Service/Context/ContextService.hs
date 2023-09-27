module Wizard.Service.Context.ContextService where

import qualified Control.Exception.Base as E
import Control.Monad (when)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.Aeson (Value (..), toJSON)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.ContextResult
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import qualified Wizard.Service.User.UserMapper as UM
import Wizard.Util.Context

runFunctionForAllTenants :: String -> AppContextM (ContextResult, Maybe String) -> AppContextM ()
runFunctionForAllTenants functionName function = do
  tenants <- findTenants
  traverse_ (\tenant -> runFunctionUnderDifferentUserAndTenant Nothing tenant.uuid functionName function) tenants

runFunctionUnderDifferentUserAndTenant
  :: Maybe User -> U.UUID -> String -> AppContextM (ContextResult, Maybe String) -> AppContextM ()
runFunctionUnderDifferentUserAndTenant mUser tenantUuid functionName function = do
  logInfoI
    _CMP_SERVICE
    ( f'
        "Running '%s' with tenant ('%s') and user ('%s') started"
        [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser]
    )
  context <- ask
  newTraceUuid <- liftIO generateUuid
  eResult <- liftIO . E.try $ runAppContextWithAppContext function (updateContext mUser tenantUuid newTraceUuid context)
  let (resultState, mReturnedMessage) =
        case eResult :: Either E.SomeException (Either String (ContextResult, Maybe String)) of
          Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
          Right (Left error) -> (ErrorContextResult, Just error)
          Left exception -> (ErrorContextResult, Just . show $ exception)
  case resultState of
    SuccessContextResult ->
      logInfoI
        _CMP_SERVICE
        ( f'
            "Running '%s' with tenant ('%s') and user ('%s') finished successfully. It returns: '%s''"
            [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser, show mReturnedMessage]
        )
    ErrorContextResult -> do
      logInfoI
        _CMP_SERVICE
        ( f'
            "Running '%s' with tenant ('%s') and user ('%s') failed. The reason is: '%s''"
            [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser, show mReturnedMessage]
        )
      sendToSentry mUser tenantUuid functionName mReturnedMessage

-- --------------------------------
-- PRIVATE
-- --------------------------------
updateContext :: Maybe User -> U.UUID -> U.UUID -> AppContext -> AppContext
updateContext mUser aUuid newTraceUuid context =
  context
    { currentTenantUuid = aUuid
    , currentUser = fmap UM.toDTO mUser
    , traceUuid = newTraceUuid
    }

sendToSentry :: Maybe User -> U.UUID -> String -> Maybe String -> AppContextM ()
sendToSentry mUser tenantUuid functionName mErrorMessage = do
  serverConfig <- asks serverConfig
  when
    serverConfig.sentry.enabled
    ( do
        let sentryDsn = serverConfig.sentry.dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks buildInfoConfig
        let buildVersion = buildInfoConfig.version
        let message = fromMaybe "" mErrorMessage
        liftIO $
          register
            sentryService
            "persistentCommandLogger"
            Error
            message
            (recordUpdate buildVersion mUser tenantUuid functionName mErrorMessage)
    )

recordUpdate :: String -> Maybe User -> U.UUID -> String -> Maybe String -> SentryRecord -> SentryRecord
recordUpdate buildVersion mUser tenantUuid functionName mErrorMessage record =
  let userUuid = maybe "anonymous" (U.toString . (.uuid)) mUser
   in record
        { srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              [("sentry.interfaces.User", toJSON $ HashMap.fromList [("id", String . T.pack $ userUuid)])]
        , srTags = HashMap.fromList [("function", functionName), ("tenantUuid", U.toString tenantUuid)]
        , srExtra =
            HashMap.fromList
              [ ("function", String . T.pack $ functionName)
              , ("tenantUuid", String . T.pack . U.toString $ tenantUuid)
              , ("lastErrorMessage", String . T.pack . fromMaybe "" $ mErrorMessage)
              ]
        }
