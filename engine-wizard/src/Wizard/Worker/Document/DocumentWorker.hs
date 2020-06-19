module Wizard.Worker.Document.DocumentWorker
  ( documentWorker
  ) where

import Control.Monad.Reader (liftIO)
import Prelude hiding (log)
import System.Cron

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
  runLogging $ do
    log "starting"
    liftIO $ runAppContextWithBaseContext cleanDocuments context
    log "ended"

log msg = logInfo _CMP_WORKER ("DocumentWorker: " ++ msg)
