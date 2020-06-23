module Wizard.Worker.Document.DocumentWorker
  ( documentWorker
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)
import System.Cron

import LensesConfig
import Wizard.Bootstrap.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService
import Wizard.Util.Logger

documentWorker :: MonadSchedule m => BaseContext -> m ()
documentWorker context = addJob (job context) "0 */4 * * *"

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanDocuments context
        log "ended"

log msg = logInfo _CMP_WORKER ("DocumentWorker: " ++ msg)
