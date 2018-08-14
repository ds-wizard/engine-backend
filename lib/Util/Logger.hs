module Util.Logger where

import Control.Monad.Logger
       (MonadLogger, logErrorN, logInfoN, logWarnN)
import qualified Data.Text as T

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . T.pack

logError :: MonadLogger m => String -> m ()
logError = logErrorN . T.pack
