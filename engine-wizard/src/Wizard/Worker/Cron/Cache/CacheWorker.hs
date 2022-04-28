module Wizard.Worker.Cron.Cache.CacheWorker
  ( cacheWorker
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import Prelude hiding (log)
import System.Cron

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Cache.CacheService
import Wizard.Util.Context
import Wizard.Util.Logger

cacheWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
cacheWorker context =
  when
    (context ^. serverConfig . cache . purgeExpired . enabled)
    (addJob (job context) (T.pack $ context ^. serverConfig . cache . purgeExpired . cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext purgeExpiredCache context
        log "ended"

-- -----------------------------------------------------------------------------
log msg = logInfo _CMP_WORKER ("CacheWorker: " ++ msg)
