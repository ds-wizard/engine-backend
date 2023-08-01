module Wizard.Worker.Permanent.PersistentCommand.PersistentCommandListenerWorker (
  persistentCommandListenerJob,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService
import Wizard.Util.Context

persistentCommandListenerJob :: BaseContext -> IO ()
persistentCommandListenerJob context =
  when context.serverConfig.persistentCommand.listenerJob.enabled (job context)

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext runPersistentCommandChannelListener' context
        log "ended"

log msg = logInfo _CMP_WORKER ("PersistentCommandWorker: " ++ msg)
