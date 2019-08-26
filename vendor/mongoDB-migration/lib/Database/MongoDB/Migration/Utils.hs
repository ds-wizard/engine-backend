module Database.MongoDB.Migration.Utils where

import Control.Monad.Logger (MonadLogger, logInfoN)
import qualified Data.Text as T

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack
