module Util.Logger where

import Control.Lens ((^.))
import Control.Monad.Logger
       (MonadLogger, logErrorN, logInfoN, logWarnN)
import Control.Monad.Reader (asks)
import Control.Monad.Reader (MonadReader)
import qualified Data.Text as T
import qualified Data.UUID as U

import LensesConfig
import Model.Context.AppContext

msg :: String -> String -> String
msg component message = component ++ ": " ++ message

-- ---------------------------------------------------------------------------
logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . T.pack

logError :: MonadLogger m => String -> m ()
logError = logErrorN . T.pack

-- ---------------------------------------------------------------------------
logInfoU :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logInfoU = logU logInfoN

logWarnU :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logWarnU = logU logWarnN

logErrorU :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logErrorU = logU logErrorN

-- ---------------------------------------------------------------------------
logU :: (MonadReader AppContext m, MonadLogger m) => (T.Text -> m ()) -> String -> m ()
logU log message = do
  mUser <- asks _appContextCurrentUser
  traceUuid <- asks _appContextTraceUuid
  case mUser of
    Just user ->
      let userUuidStamp = createUserUuidLoggerStamp (U.toString $ user ^. uuid)
          traceUuidStamp = createTraceUuidLoggerStamp (U.toString $ traceUuid)
          composedMessage = (userUuidStamp ++ traceUuidStamp) ++ " " ++ message
      in log . T.pack $ composedMessage
    Nothing ->
      let userUuidStamp = createUserUuidLoggerStamp "Anonymous"
          traceUuidStamp = createTraceUuidLoggerStamp (U.toString $ traceUuid)
          composedMessage = (userUuidStamp ++ traceUuidStamp) ++ " " ++ message
      in log . T.pack $ composedMessage

-- ---------------------------------------------------------------------------
createLoggerStamp :: String -> String -> String
createLoggerStamp label value = "[" ++ label ++ ":" ++ value ++ "]"

createUserUuidLoggerStamp :: String -> String
createUserUuidLoggerStamp = createLoggerStamp "U"

createTraceUuidLoggerStamp :: String -> String
createTraceUuidLoggerStamp = createLoggerStamp "T"
