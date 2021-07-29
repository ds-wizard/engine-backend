module Wizard.Worker.Cron.Questionnaire.SquashQuestionnaireEventsWorker
  ( squashQuestionnaireEventsWorker
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import Prelude hiding (log)
import System.Cron hiding (cron)

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService hiding (squash)
import Wizard.Util.Context
import Wizard.Util.Logger

squashQuestionnaireEventsWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
squashQuestionnaireEventsWorker context =
  when
    (context ^. serverConfig . questionnaire . squash . enabled)
    (addJob (job context) (T.pack $ context ^. serverConfig . questionnaire . squash . cron))

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
