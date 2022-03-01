module Shared.Util.Logger
  ( logDebugI
  , logInfoI
  , logWarnI
  , logErrorI
  , logDebug
  , logInfo
  , logWarn
  , logError
  , createLogRecord
  , showLogLevel
  , runLogging
  , f'
  , LogLevel(..)
  , module Shared.Constant.Component
  ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
  ( LogLevel(..)
  , LogSource(..)
  , LoggingT(..)
  , MonadLogger
  , filterLogger
  , logWithoutLoc
  , runStdoutLoggingT
  )
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import Prelude hiding (log)
import System.Console.Pretty (Color(..), color)

import Shared.Constant.Component
import Shared.Model.Context.ContextLenses
import Shared.Util.String (f')

-- ---------------------------------------------------------------------------
logDebugI :: (MonadReader s m, HasIdentityUuid' s, HasTraceUuid' s, MonadLogger m) => String -> String -> m ()
logDebugI = logI LevelDebug

logInfoI :: (MonadReader s m, HasIdentityUuid' s, HasTraceUuid' s, MonadLogger m) => String -> String -> m ()
logInfoI = logI LevelInfo

logWarnI :: (MonadReader s m, HasIdentityUuid' s, HasTraceUuid' s, MonadLogger m) => String -> String -> m ()
logWarnI = logI LevelWarn

logErrorI :: (MonadReader s m, HasIdentityUuid' s, HasTraceUuid' s, MonadLogger m) => String -> String -> m ()
logErrorI = logI LevelError

logI :: (MonadReader s m, HasIdentityUuid' s, HasTraceUuid' s, MonadLogger m) => LogLevel -> String -> String -> m ()
logI logLevel component message = do
  context <- ask
  let mIdentityUuid = context ^. identityUuid'
  let mTraceUuid = Just . U.toString $ context ^. traceUuid'
  let record = createLogRecord logLevel mIdentityUuid mTraceUuid component message
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

showLogLevel :: LogLevel -> String
showLogLevel LevelDebug = "Debug"
showLogLevel LevelInfo = "Info "
showLogLevel LevelWarn = "Warn "
showLogLevel LevelError = "Error"
