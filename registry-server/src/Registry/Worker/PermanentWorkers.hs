module Registry.Worker.PermanentWorkers where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextMappers
import Registry.Service.PersistentCommand.PersistentCommandService
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger

permanentWorker :: BaseContext -> LoggingT IO [ThreadId]
permanentWorker context = do
  threadId <- liftIO $ forkIO (persistentCommandListenerJob context)
  return [threadId]

-- -----------------------------------------------------------------------------
-- WORKERS
-- -----------------------------------------------------------------------------
persistentCommandListenerJob :: BaseContext -> IO ()
persistentCommandListenerJob context =
  when
    context.serverConfig.persistentCommand.listenerJob.enabled
    ( do
        let loggingLevel = context.serverConfig.logging.level
         in runLogging loggingLevel $ do
              logInfo _CMP_WORKER "PersistentCommandWorker: starting"
              liftIO $ runAppContextWithBaseContext runPersistentCommandChannelListener' context
              logInfo _CMP_WORKER "PersistentCommandWorker: ended"
    )
