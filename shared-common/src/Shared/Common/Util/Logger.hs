module Shared.Common.Util.Logger (
  logDebugI,
  logInfoI,
  logWarnI,
  logErrorI,
  logDebug,
  logInfo,
  logWarn,
  logError,
  createLogRecord,
  showLogLevel,
  runLogging,
  f',
  LogLevel (..),
  module Shared.Common.Constant.Component,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LogLevel (..), LogSource (..), LoggingT (..), MonadLogger, filterLogger, logWithoutLoc, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Aeson (Value (..), toJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import GHC.Records
import System.Console.Pretty (Color (..), color)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))
import Prelude hiding (log)

import Shared.Common.Constant.Component
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig hiding (email)
import Shared.Common.Util.String (f')

-- ---------------------------------------------------------------------------
logDebugI :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logDebugI = logI LevelDebug

logInfoI :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logInfoI = logI LevelInfo

logWarnI :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logWarnI = logI LevelWarn

logErrorI
  :: ( MonadReader s m
     , MonadLogger m
     , MonadIO m
     , HasField "identity'" s (Maybe String)
     , HasField "identityEmail'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => String
  -> String
  -> m ()
logErrorI component message = do
  logI LevelError component message
  sendToSentry component message

logI :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => LogLevel -> String -> String -> m ()
logI logLevel component message = do
  context <- ask
  let mTraceUuid = Just . U.toString $ context.traceUuid'
  let record = createLogRecord logLevel context.identity' mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

-- ---------------------------------------------------------------------------
logDebug :: MonadLogger m => String -> String -> m ()
logDebug = log LevelDebug

logInfo :: MonadLogger m => String -> String -> m ()
logInfo = log LevelInfo

logWarn :: MonadLogger m => String -> String -> m ()
logWarn = log LevelWarn

logError :: MonadLogger m => String -> String -> m ()
logError = log LevelError

-- ---------------------------------------------------------------------------
runLogging :: MonadIO m => LogLevel -> LoggingT m a -> m a
runLogging level = runStdoutLoggingT . filterLogger (filterAppLogging level)

filterAppLogging :: LogLevel -> LogSource -> LogLevel -> Bool
filterAppLogging LevelDebug _ (LevelOther "Debug") = True
filterAppLogging LevelDebug _ (LevelOther "Info ") = True
filterAppLogging LevelDebug _ (LevelOther "Warn ") = True
filterAppLogging LevelDebug _ (LevelOther "Error") = True
filterAppLogging LevelInfo _ (LevelOther "Debug") = False
filterAppLogging LevelInfo _ (LevelOther "Info ") = True
filterAppLogging LevelInfo _ (LevelOther "Warn ") = True
filterAppLogging LevelInfo _ (LevelOther "Error") = True
filterAppLogging LevelWarn _ (LevelOther "Debug") = False
filterAppLogging LevelWarn _ (LevelOther "Info ") = False
filterAppLogging LevelWarn _ (LevelOther "Warn ") = True
filterAppLogging LevelWarn _ (LevelOther "Error") = True
filterAppLogging LevelError _ (LevelOther "Debug") = False
filterAppLogging LevelError _ (LevelOther "Info ") = False
filterAppLogging LevelError _ (LevelOther "Warn ") = False
filterAppLogging LevelError _ (LevelOther "Error") = True
filterAppLogging _ _ _ = False

-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------
log logLevel component message =
  let record = createLogRecord logLevel Nothing Nothing component message
   in logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

-- ---------------------------------------------------------------------------
createLogRecord :: LogLevel -> Maybe String -> Maybe String -> String -> String -> String
createLogRecord logLevel mUserUuid mTraceUuid component message = color recordColor record
  where
    userUuidStamp = createLoggerStamp "I" (fromMaybe "------------------------------------" mUserUuid)
    traceUuidStamp = createLoggerStamp "T" (fromMaybe "------------------------------------" mTraceUuid)
    componentStamp = createLoggerStamp "" component
    record = L.intercalate "" [userUuidStamp, " ", traceUuidStamp, " ", componentStamp, " ", message]
    recordColor = getColor logLevel

createLoggerStamp :: String -> String -> String
createLoggerStamp "" value = "[" ++ value ++ "]"
createLoggerStamp label value = "[" ++ label ++ ":" ++ value ++ "]"

getColor :: LogLevel -> Color
getColor LevelDebug = Default
getColor LevelInfo = Default
getColor LevelWarn = Magenta
getColor LevelError = Red
getColor (LevelOther _) = Default

showLogLevel :: LogLevel -> String
showLogLevel LevelDebug = "Debug"
showLogLevel LevelInfo = "Info "
showLogLevel LevelWarn = "Warn "
showLogLevel LevelError = "Error"
showLogLevel (LevelOther level) = T.unpack level

-- ---------------------------------------------------------------------------
sendToSentry
  :: ( MonadReader s m
     , MonadLogger m
     , MonadIO m
     , HasField "identity'" s (Maybe String)
     , HasField "identityEmail'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => String
  -> String
  -> m ()
sendToSentry component message = do
  context <- ask
  when
    (context.serverConfig'.sentry'.enabled)
    ( do
        let sentryDsn = context.serverConfig'.sentry'.dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        let buildVersion = context.buildInfoConfig'.version
        liftIO $ register sentryService "messageLogger" Error message (recordUpdate buildVersion context.identity' context.identityEmail' context.traceUuid')
    )

recordUpdate :: String -> Maybe String -> Maybe String -> U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion mIdentity mEmail traceUuid record =
  record
    { srRelease = Just buildVersion
    , srInterfaces =
        HashMap.fromList
          [
            ( "sentry.interfaces.User"
            , toJSON $ HashMap.fromList [("id", String . T.pack . fromMaybe "" $ mIdentity), ("email", String . T.pack . fromMaybe "" $ mEmail)]
            )
          ]
    , srTags = HashMap.fromList [("traceUuid", U.toString traceUuid)]
    , srExtra = HashMap.fromList [("traceUuid", String . T.pack . U.toString $ traceUuid)]
    }
