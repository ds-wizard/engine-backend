module Wizard.Worker.Cron.Plan.AppPlanWorker (
  appPlanWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron hiding (cron)
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Plan.AppPlanService
import Wizard.Util.Context

appPlanWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
appPlanWorker context =
  when
    (jobEnabled && cloudEnabled)
    (addJob (job context) (T.pack $ context.serverConfig.plan.recomputeJob.cron))
  where
    jobEnabled = context.serverConfig.plan.recomputeJob.enabled
    cloudEnabled = context.serverConfig.cloud.enabled

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext recomputePlansForApps context
        log "ended"

log msg = logInfo _CMP_WORKER ("AppPlanWorker: " ++ msg)
