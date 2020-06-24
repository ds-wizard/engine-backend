module Registry.Util.Logger
  ( logDebugU
  , logInfoU
  , logWarnU
  , logErrorU
  , LogLevel(..)
  , module Shared.Constant.Component
  , module Shared.Util.Logger
  ) where

import Control.Monad.Logger (MonadLogger, logWithoutLoc)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Text as T
import qualified Data.UUID as U
import Prelude hiding (log)

import Registry.Model.Context.AppContext
import Registry.Model.Organization.Organization
import Shared.Constant.Component
import Shared.Util.Logger

logDebugU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logDebugU = logU LevelDebug

logInfoU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logInfoU = logU LevelInfo

logWarnU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logWarnU = logU LevelWarn

logErrorU :: (MonadReader AppContext m, MonadLogger m) => String -> String -> m ()
logErrorU = logU LevelError

-- ---------------------------------------------------------------------------
logU :: (MonadReader AppContext m, MonadLogger m) => LogLevel -> String -> String -> m ()
logU logLevel component message = do
  mOrg <- asks _appContextCurrentOrganization
  traceUuid <- asks _appContextTraceUuid
  let mOrgToken = fmap _organizationToken mOrg
  let mTraceUuid = Just . U.toString $ traceUuid
  let record = createLogRecord logLevel mOrgToken mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record
