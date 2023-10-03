module Registry.Worker.Permanent.PersistentCommand.PersistentCommandListenerWorker (
  persistentCommandListenerJob,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Registry.Service.PersistentCommand.PersistentCommandService
import Registry.Util.Context
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger

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
