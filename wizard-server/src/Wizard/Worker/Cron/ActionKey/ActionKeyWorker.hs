module Wizard.Worker.Cron.ActionKey.ActionKeyWorker (
  actionKeyWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Util.Context
import Wizard.Util.Logger

actionKeyWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
actionKeyWorker context =
  when
    context.serverConfig.actionKey.clean.enabled
    (addJob (job context) (T.pack $ context.serverConfig.actionKey.clean.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanActionKeys context
        log "ended"

log msg = logInfo _CMP_WORKER ("ActionKeyWorker: " ++ msg)
