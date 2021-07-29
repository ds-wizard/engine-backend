module Wizard.Worker.Cron.Questionnaire.SquashQuestionnaireEventsWorker
  ( squashQuestionnaireEventsWorker
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)
import System.Cron

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService
import Wizard.Util.Context
import Wizard.Util.Logger

squashQuestionnaireEventsWorker :: MonadSchedule m => BaseContext -> m ()
squashQuestionnaireEventsWorker context = addJob (job context) "* * * * *"

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext squashQuestionnaireEvents context
        log "ended"

log msg = logInfo _CMP_WORKER ("SquashQuestionnaireEventsWorker: " ++ msg)
