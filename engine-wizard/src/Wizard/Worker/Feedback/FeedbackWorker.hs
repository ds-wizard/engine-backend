module Wizard.Worker.Feedback.FeedbackWorker
  ( feedbackWorker
  ) where

import Control.Monad.Reader (liftIO)
import Prelude hiding (log)
import System.Cron

import Wizard.Bootstrap.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService
import Wizard.Util.Logger

feedbackWorker :: MonadSchedule m => BaseContext -> m ()
feedbackWorker context = addJob (run context) "0 2 * * *"

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
run :: BaseContext -> IO ()
run context =
  runLogging $ do
    log "starting"
    liftIO $ runAppContextWithBaseContext synchronizeFeedbacks context
    log "ended"

-- -----------------------------------------------------------------------------
log msg = logInfo _CMP_WORKER ("FeedbackWorker: " ++ msg)
