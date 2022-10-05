module Wizard.Worker.Cron.Questionnaire.RecomputeQuestionnaireIndicationWorker
  ( recomputeQuestionnaireIndicationWorker
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import Prelude hiding (log)
import System.Cron hiding (cron)

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Util.Context
import Wizard.Util.Logger

recomputeQuestionnaireIndicationWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
recomputeQuestionnaireIndicationWorker context =
  when
    (context ^. serverConfig . questionnaire . recomputeIndication . enabled)
    (addJob (job context) (T.pack $ context ^. serverConfig . questionnaire . recomputeIndication . cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext recomputeQuestionnaireIndications context
        log "ended"

log msg = logInfo _CMP_WORKER ("RecomputeIndicationWorker: " ++ msg)
