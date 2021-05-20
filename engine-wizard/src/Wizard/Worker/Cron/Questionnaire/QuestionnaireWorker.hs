module Wizard.Worker.Cron.Questionnaire.QuestionnaireWorker
  ( questionnaireWorker
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)
import System.Cron

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Util.Context
import Wizard.Util.Logger

questionnaireWorker :: MonadSchedule m => BaseContext -> m ()
questionnaireWorker context = addJob (job context) "31 */4 * * *"

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanQuestionnaires context
        log "ended"

log msg = logInfo _CMP_WORKER ("QuestionnaireWorker: " ++ msg)
