module Shared.Worker.Bootstrap.Worker (
  worker,
) where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import System.Cron
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Context.BaseContext
import Shared.Common.Util.Logger
import Shared.Worker.Model.Worker.CronWorker

worker
  :: ( AppContextC s sc appContextM
     , BaseContextType baseContext sc
     )
  => (appContextM () -> baseContext -> IO (Either String ()))
  -> (appContextM () -> baseContext -> IO (Either String ()))
  -> MVar ()
  -> baseContext
  -> [CronWorker baseContext appContextM]
  -> (baseContext -> LoggingT IO [ThreadId])
  -> IO ()
worker runAppContextWithBaseContext runAppContextWithBaseContext'' shutdownFlag context cronWorkers permanentWorker =
  let loggingLevel = context.serverConfig'.logging.level
   in runLogging loggingLevel $ do
        cronWorkerThreadIds <- cronJob runAppContextWithBaseContext runAppContextWithBaseContext'' context cronWorkers
        permanentWorkerThreadIds <- permanentWorker context
        setupHandlers loggingLevel shutdownFlag (cronWorkerThreadIds ++ permanentWorkerThreadIds)

-- ------------------------------------------------------------------
cronJob
  :: ( AppContextC s sc appContextM
     , BaseContextType baseContext sc
     )
  => (appContextM () -> baseContext -> IO (Either String ()))
  -> (appContextM () -> baseContext -> IO (Either String ()))
  -> baseContext
  -> [CronWorker baseContext appContextM]
  -> LoggingT IO [ThreadId]
cronJob runAppContextWithBaseContext runAppContextWithBaseContext'' context workers = do
  logInfo _CMP_WORKER "scheduling workers started"
  threadIds <- liftIO . execSchedule $ traverse_ (workerFn runAppContextWithBaseContext runAppContextWithBaseContext'' context) workers
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

workerFn
  :: ( MonadSchedule m
     , Applicative m
     , AppContextC s sc appContextM
     , BaseContextType baseContext sc
     )
  => (appContextM () -> baseContext -> IO (Either String ()))
  -> (appContextM () -> baseContext -> IO (Either String ()))
  -> baseContext
  -> CronWorker baseContext appContextM
  -> m ()
workerFn runAppContextWithBaseContext runAppContextWithBaseContext'' context cronWorker =
  when
    (cronWorker.condition context)
    (addJob (job runAppContextWithBaseContext runAppContextWithBaseContext'' cronWorker context) (T.pack . cronWorker.cron $ context))

job
  :: ( AppContextC appContext sc appContextM
     , BaseContextType baseContext sc
     )
  => (appContextM () -> baseContext -> IO (Either String ()))
  -> (appContextM () -> baseContext -> IO (Either String ()))
  -> CronWorker baseContext appContextM
  -> baseContext
  -> IO ()
job runAppContextWithBaseContext runAppContextWithBaseContext'' cronWorker context =
  let loggingLevel = context.serverConfig'.logging.level
   in runLogging loggingLevel $ do
        logInfo _CMP_WORKER . f' "%s: starting" $ [cronWorker.name]
        if cronWorker.wrapInTransaction
          then liftIO $ runAppContextWithBaseContext cronWorker.function context
          else liftIO $ runAppContextWithBaseContext'' cronWorker.function context
        logInfo _CMP_WORKER . f' "%s: ended" $ [cronWorker.name]
