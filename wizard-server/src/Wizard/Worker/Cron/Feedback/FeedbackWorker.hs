module Wizard.Worker.Cron.Feedback.FeedbackWorker (
  feedbackWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService
import Wizard.Util.Context

feedbackWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
feedbackWorker context =
  when
    context.serverConfig.feedback.sync.enabled
    (addJob (job context) (T.pack $ context.serverConfig.feedback.sync.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext synchronizeFeedbacksInAllApplications context
        log "ended"

-- -----------------------------------------------------------------------------
log msg = logInfo _CMP_WORKER ("FeedbackWorker: " ++ msg)
