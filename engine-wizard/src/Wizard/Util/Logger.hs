module Wizard.Util.Logger
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
import Shared.Constant.Component
import Shared.Util.Logger
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext

logDebugU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logDebugU = logU LevelDebug

logInfoU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logInfoU = logU LevelInfo

logWarnU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logWarnU = logU LevelWarn

logErrorU :: String -> String -> AppContextM ()
logErrorU component message = do
  logU LevelError component message
  sendToSentry component message

-- ---------------------------------------------------------------------------
logU :: (MonadReader AppContext m, MonadLogger m) => LogLevel -> String -> String -> m ()
logU logLevel component message = do
  mUser <- asks _appContextCurrentUser
  traceUuid <- asks _appContextTraceUuid
  let mUserUuid = fmap (U.toString . _userDTOUuid) mUser
  let mTraceUuid = Just . U.toString $ traceUuid
  let record = createLogRecord logLevel mUserUuid mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

sendToSentry :: String -> String -> AppContextM ()
sendToSentry component message = do
  mUser <- asks _appContextCurrentUser
  traceUuid <- asks _appContextTraceUuid
  let mIdentityUuid = fmap (U.toString . _userDTOUuid) mUser
  serverConfig <- asks _appContextServerConfig
  when
    (serverConfig ^. sentry . enabled)
    (do let sentryDsn = serverConfig ^. sentry . dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks _appContextBuildInfoConfig
        let buildVersion = buildInfoConfig ^. version
        appUuid <- asks _appContextAppUuid
        liftIO $
          register sentryService "appLogger" Error message (recordUpdate buildVersion mIdentityUuid traceUuid appUuid))

recordUpdate :: String -> Maybe String -> U.UUID -> U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion mIdentityUuid traceUuid appUuid record =
  record
    { srRelease = Just buildVersion
    , srExtra =
        HashMap.fromList
          [ ("identityUuid", String . T.pack . fromMaybe "" $ mIdentityUuid)
          , ("traceUuid", String . T.pack . U.toString $ traceUuid)
          , ("appUuid", String . T.pack . U.toString $ appUuid)
          ]
    , srTags =
        HashMap.fromList
          [ ("identityUuid", fromMaybe "" mIdentityUuid)
          , ("traceUuid", U.toString traceUuid)
          , ("appUuid", U.toString appUuid)
          ]
    }
