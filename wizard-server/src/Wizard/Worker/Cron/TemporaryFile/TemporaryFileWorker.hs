module Wizard.Worker.Cron.TemporaryFile.TemporaryFileWorker (
  temporaryFileWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.TemporaryFile.TemporaryFileService
import Wizard.Util.Context
import Wizard.Util.Logger

temporaryFileWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
temporaryFileWorker context =
  when
    context.serverConfig.temporaryFile.clean.enabled
    (addJob (job context) (T.pack $ context.serverConfig.temporaryFile.clean.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanTemporaryFiles context
        log "ended"

log msg = logInfo _CMP_WORKER ("TemporaryFileWorker: " ++ msg)
