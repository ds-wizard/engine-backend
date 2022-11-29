module Wizard.Worker.Cron.Branch.SquashBranchEventsWorker (
  squashBranchEventsWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron hiding (cron)
import Prelude hiding (log)

import Shared.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.Event.BranchEventService hiding (squash)
import Wizard.Util.Context
import Wizard.Util.Logger

squashBranchEventsWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
squashBranchEventsWorker context =
  when
    (context.serverConfig.branch.squash.enabled)
    (addJob (job context) (T.pack context.serverConfig.branch.squash.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext squashEvents context
        log "ended"

log msg = logInfo _CMP_WORKER ("SquashBranchEventsWorker: " ++ msg)
