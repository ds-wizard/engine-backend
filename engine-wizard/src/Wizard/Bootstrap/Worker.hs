module Wizard.Bootstrap.Worker
  ( cronJob
  ) where

import Control.Concurrent
import Control.Monad.Reader (liftIO)
import System.Cron
import System.Posix.Signals (Handler(CatchOnce), installHandler, sigINT, sigTERM)

import Wizard.Model.Context.BaseContext
import Wizard.Util.Logger
import Wizard.Worker.Document.DocumentWorker
import Wizard.Worker.Feedback.FeedbackWorker

cronJob :: MVar () -> BaseContext -> IO ()
cronJob shutdownFlag context =
  runLogging $ do
    logInfo _CMP_WORKER "sheduling workers started"
    threadIds <-
      liftIO . execSchedule $ do
        feedbackWorker context
        documentWorker context
    setupHandlers shutdownFlag threadIds
    logInfo _CMP_WORKER "sheduling workers completed"

setupHandlers shutdownFlag threadIds = do
  logInfo _CMP_WORKER "installing handlers"
  liftIO $ installHandler sigINT (handler shutdownFlag threadIds "sigINT") Nothing
  liftIO $ installHandler sigTERM (handler shutdownFlag threadIds "sigTERM") Nothing
  logInfo _CMP_WORKER "handlers installed"

handler shutdownFlag threadIds typeSignal =
  CatchOnce . runLogging $ do
    logInfo _CMP_WORKER "shuting down workers: started"
    liftIO $ traverse killThread threadIds
    logInfo _CMP_WORKER "shuting down workers: notifing web server"
    liftIO $ putMVar shutdownFlag ()
    logInfo _CMP_WORKER "shuting down workers: completed"
