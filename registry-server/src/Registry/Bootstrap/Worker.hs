module Registry.Bootstrap.Worker (
  worker,
) where

import Control.Concurrent
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import System.Cron
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Registry.Worker.Cron.PersistentCommand.PersistentCommandRetryWorker
import Registry.Worker.Permanent.PersistentCommand.PersistentCommandListenerWorker
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger

worker :: MVar () -> BaseContext -> IO ()
worker shutdownFlag context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        cronWorkerThreadIds <- cronJob context
        permanentWorkerThreadIds <- permanentWorker context
        setupHandlers loggingLevel shutdownFlag (cronWorkerThreadIds ++ permanentWorkerThreadIds)

-- ------------------------------------------------------------------
permanentWorker :: BaseContext -> LoggingT IO [ThreadId]
permanentWorker context = do
  threadId <- liftIO $ forkIO (persistentCommandListenerJob context)
  return [threadId]

-- ------------------------------------------------------------------
cronJob :: BaseContext -> LoggingT IO [ThreadId]
cronJob context = do
  logInfo _CMP_WORKER "scheduling workers started"
  threadIds <-
    liftIO . execSchedule $ persistentCommandRetryWorker context
  logInfo _CMP_WORKER "scheduling workers completed"
  return threadIds

setupHandlers loggingLevel shutdownFlag threadIds = do
  logInfo _CMP_WORKER "installing handlers"
  liftIO $ installHandler sigINT (handler loggingLevel shutdownFlag threadIds "sigINT") Nothing
  liftIO $ installHandler sigTERM (handler loggingLevel shutdownFlag threadIds "sigTERM") Nothing
  logInfo _CMP_WORKER "handlers installed"

handler loggingLevel shutdownFlag threadIds typeSignal =
  CatchOnce . runLogging loggingLevel $ do
    logInfo _CMP_WORKER "shutting down workers: started"
    liftIO $ traverse killThread threadIds
    logInfo _CMP_WORKER "shutting down workers: notifing web server"
    liftIO $ putMVar shutdownFlag ()
    logInfo _CMP_WORKER "shutting down workers: completed"
