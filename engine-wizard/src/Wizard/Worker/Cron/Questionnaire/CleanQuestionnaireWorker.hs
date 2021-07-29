module Wizard.Worker.Cron.Questionnaire.CleanQuestionnaireWorker
  ( cleanQuestionnaireWorker
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

cleanQuestionnaireWorker :: MonadSchedule m => BaseContext -> m ()
cleanQuestionnaireWorker context = addJob (job context) "31 */4 * * *"

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanQuestionnaires context
        log "ended"

log msg = logInfo _CMP_WORKER ("CleanQuestionnaireWorker: " ++ msg)
