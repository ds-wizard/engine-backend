module Wizard.Worker.Permanent.Questionnaire.QuestionnaireWorker where

import Control.Concurrent
import Control.Monad (void)
import Prelude hiding (log)

import Wizard.Messaging.In.Queue.Questionnaire
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Util.Context
import Wizard.Util.Logger

questionnaireWorker :: MVar () -> BaseContext -> IO ()
questionnaireWorker shutdownFlag context = void $ runAppContextWithBaseContext (job shutdownFlag) context

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: MVar () -> AppContextM ()
job shutdownFlag = do
  log "starting"
  consumeQuestionnaireEventsQueue
  return ()

log msg = logInfo _CMP_WORKER ("QuestionnaireWorker: " ++ msg)
