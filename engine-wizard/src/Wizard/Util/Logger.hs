module Wizard.Util.Logger where

import Control.Lens ((^.))
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN, logWarnN)
import Control.Monad.Reader (asks)
import Control.Monad.Reader (MonadReader)
import qualified Data.Text as T
import qualified Data.UUID as U
import System.Console.Pretty (Color(..), color)

import LensesConfig hiding (color)
import Wizard.Model.Context.AppContext

msg :: String -> String -> String
msg component message = component ++ ": " ++ message

-- ---------------------------------------------------------------------------
logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . color Magenta . T.pack

logError :: MonadLogger m => String -> m ()
logError = logErrorN . color Red . T.pack

-- ---------------------------------------------------------------------------
logInfoU :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logInfoU = logU Default logInfoN

logWarnU :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logWarnU = logU Magenta logWarnN

logErrorU :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logErrorU = logU Red logErrorN

-- ---------------------------------------------------------------------------
logU :: (MonadReader AppContext m, MonadLogger m) => Color -> (T.Text -> m ()) -> String -> m ()
logU c log message = do
  mUser <- asks _appContextCurrentUser
  traceUuid <- asks _appContextTraceUuid
  case mUser of
    Just user ->
      let userUuidStamp = createUserUuidLoggerStamp (U.toString $ user ^. uuid)
          traceUuidStamp = createTraceUuidLoggerStamp (U.toString $ traceUuid)
          composedMessage = (userUuidStamp ++ traceUuidStamp) ++ " " ++ message
       in log . color c . T.pack $ composedMessage
    Nothing ->
      let userUuidStamp = createUserUuidLoggerStamp "Anonymous"
          traceUuidStamp = createTraceUuidLoggerStamp (U.toString $ traceUuid)
          composedMessage = (userUuidStamp ++ traceUuidStamp) ++ " " ++ message
       in log . color c . T.pack $ composedMessage

-- ---------------------------------------------------------------------------
createLoggerStamp :: String -> String -> String
createLoggerStamp label value = "[" ++ label ++ ":" ++ value ++ "]"

createUserUuidLoggerStamp :: String -> String
createUserUuidLoggerStamp = createLoggerStamp "U"

createTraceUuidLoggerStamp :: String -> String
createTraceUuidLoggerStamp = createLoggerStamp "T"
