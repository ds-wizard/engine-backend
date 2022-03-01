module Registry.Util.Logger
  ( logDebugU
  , logInfoU
  , logWarnU
  , logErrorU
  , LogLevel(..)
  , module Shared.Constant.Component
  , module Shared.Util.Logger
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Logger (MonadLogger, logWithoutLoc)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import Prelude hiding (log)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel(Error), SentryRecord(..))

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Organization.Organization
import Shared.Constant.Component
import Shared.Util.Logger

logDebugU :: String -> String -> AppContextM ()
logDebugU = logU LevelDebug

logInfoU :: String -> String -> AppContextM ()
logInfoU = logU LevelInfo

logWarnU :: String -> String -> AppContextM ()
logWarnU = logU LevelWarn

logErrorU :: String -> String -> AppContextM ()
logErrorU component message = do
  logU LevelError component message
  sendToSentry component message

-- ---------------------------------------------------------------------------
logU :: (MonadReader AppContext m, MonadLogger m) => LogLevel -> String -> String -> m ()
logU logLevel component message = do
  mOrg <- asks _appContextCurrentOrganization
  traceUuid <- asks _appContextTraceUuid
  let mOrgToken = fmap _organizationToken mOrg
  let mTraceUuid = Just . U.toString $ traceUuid
  let record = createLogRecord logLevel mOrgToken mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

sendToSentry :: String -> String -> AppContextM ()
sendToSentry component message = do
  mOrg <- asks _appContextCurrentOrganization
  traceUuid <- asks _appContextTraceUuid
  let mIdentityUuid = fmap _organizationToken mOrg
  serverConfig <- asks _appContextServerConfig
  when
    (serverConfig ^. sentry . enabled)
    (do let sentryDsn = serverConfig ^. sentry . dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks _appContextBuildInfoConfig
        let buildVersion = buildInfoConfig ^. version
        liftIO $ register sentryService "appLogger" Error message (recordUpdate buildVersion mIdentityUuid traceUuid))

recordUpdate :: String -> Maybe String -> U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion mIdentityUuid traceUuid record =
  record
    { srRelease = Just buildVersion
    , srExtra =
        HashMap.fromList
          [ ("identityUuid", String . T.pack . fromMaybe "" $ mIdentityUuid)
          , ("traceUuid", String . T.pack . U.toString $ traceUuid)
          ]
    , srTags = HashMap.fromList [("identityUuid", fromMaybe "" mIdentityUuid), ("traceUuid", U.toString traceUuid)]
    }
