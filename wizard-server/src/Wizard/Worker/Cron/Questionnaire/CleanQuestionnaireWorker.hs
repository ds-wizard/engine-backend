module Wizard.Worker.Cron.Questionnaire.CleanQuestionnaireWorker (
  cleanQuestionnaireWorker,
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
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Util.Context

cleanQuestionnaireWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
cleanQuestionnaireWorker context =
  when
    context.serverConfig.questionnaire.clean.enabled
    (addJob (job context) (T.pack $ context.serverConfig.questionnaire.clean.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanQuestionnaires context
        log "ended"

log msg = logInfo _CMP_WORKER ("CleanQuestionnaireWorker: " ++ msg)
