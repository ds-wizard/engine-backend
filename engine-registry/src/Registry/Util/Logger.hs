module Registry.Util.Logger where

import Control.Lens ((^.))
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN, logWarnN)
import Control.Monad.Reader (asks)
import Control.Monad.Reader (MonadReader)
import qualified Data.Text as T
import qualified Data.UUID as U

import Registry.LensesConfig
import Registry.Model.Context.AppContext

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
logInfoO :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logInfoO = logU logInfoN

logWarnO :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logWarnO = logU logWarnN

logErrorO :: (MonadReader AppContext m, MonadLogger m) => String -> m ()
logErrorO = logU logErrorN

-- ---------------------------------------------------------------------------
logU :: (MonadReader AppContext m, MonadLogger m) => (T.Text -> m ()) -> String -> m ()
logU log message = do
  mOrg <- asks _appContextCurrentOrganization
  traceUuid <- asks _appContextTraceUuid
  case mOrg of
    Just org ->
      let orgTokenStamp = createOrgTokenLoggerStamp (org ^. token)
          traceUuidStamp = createTraceUuidLoggerStamp (U.toString $ traceUuid)
          composedMessage = (orgTokenStamp ++ traceUuidStamp) ++ " " ++ message
       in log . T.pack $ composedMessage
    Nothing ->
      let orgTokenStamp = createOrgTokenLoggerStamp "Anonymous"
          traceUuidStamp = createTraceUuidLoggerStamp (U.toString $ traceUuid)
          composedMessage = (orgTokenStamp ++ traceUuidStamp) ++ " " ++ message
       in log . T.pack $ composedMessage

-- ---------------------------------------------------------------------------
createLoggerStamp :: String -> String -> String
createLoggerStamp label value = "[" ++ label ++ ":" ++ value ++ "]"

createOrgTokenLoggerStamp :: String -> String
createOrgTokenLoggerStamp = createLoggerStamp "O"

createTraceUuidLoggerStamp :: String -> String
createTraceUuidLoggerStamp = createLoggerStamp "T"
