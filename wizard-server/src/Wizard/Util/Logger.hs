module Wizard.Util.Logger (
  logDebugU,
  logInfoU,
  logWarnU,
  logErrorU,
  LogLevel (..),
  module Shared.Common.Constant.Component,
  module Shared.Common.Util.Logger,
) where

import Control.Monad (when)
import Control.Monad.Logger (logWithoutLoc)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (Value (..), toJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.UUID as U
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))
import Prelude hiding (log)

import Shared.Common.Constant.Component
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext

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
logU :: LogLevel -> String -> String -> AppContextM ()
logU logLevel component message = do
  mUser <- asks currentUser
  traceUuid <- asks traceUuid
  let mUserUuid = fmap (U.toString . uuid) mUser
  let mTraceUuid = Just . U.toString $ traceUuid
  let record = createLogRecord logLevel mUserUuid mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

sendToSentry :: String -> String -> AppContextM ()
sendToSentry component message = do
  mUser <- asks currentUser
  traceUuid <- asks traceUuid
  serverConfig <- asks serverConfig
  when
    serverConfig.sentry.enabled
    ( do
        let sentryDsn = serverConfig.sentry.dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks buildInfoConfig
        let buildVersion = buildInfoConfig.version
        appUuid <- asks currentAppUuid
        liftIO $
          register sentryService "messageLogger" Error message (recordUpdate buildVersion mUser traceUuid appUuid)
    )

recordUpdate :: String -> Maybe UserDTO -> U.UUID -> U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion mUser traceUuid appUuid record =
  let userUuid = T.pack . maybe "" (U.toString . uuid) $ mUser
      userEmail = T.pack . maybe "" (.email) $ mUser
   in record
        { srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              [
                ( "sentry.interfaces.User"
                , toJSON $ HashMap.fromList [("id", String userUuid), ("email", String userEmail)]
                )
              ]
        , srTags = HashMap.fromList [("traceUuid", U.toString traceUuid), ("appUuid", U.toString appUuid)]
        , srExtra =
            HashMap.fromList
              [ ("traceUuid", String . T.pack . U.toString $ traceUuid)
              , ("appUuid", String . T.pack . U.toString $ appUuid)
              ]
        }
