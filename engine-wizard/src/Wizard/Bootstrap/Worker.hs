module Wizard.Bootstrap.Worker
  ( cronJob
  ) where

import Control.Concurrent
import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import System.Cron
import System.Posix.Signals (Handler(CatchOnce), installHandler, sigINT, sigTERM)

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Util.Logger
import Wizard.Worker.Document.DocumentWorker
import Wizard.Worker.Feedback.FeedbackWorker

cronJob :: MVar () -> BaseContext -> IO ()
cronJob shutdownFlag context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        logInfo _CMP_WORKER "sheduling workers started"
        threadIds <-
          liftIO . execSchedule $ do
            feedbackWorker context
            documentWorker context
        setupHandlers loggingLevel shutdownFlag threadIds
        logInfo _CMP_WORKER "sheduling workers completed"

setupHandlers loggingLevel shutdownFlag threadIds = do
  logInfo _CMP_WORKER "installing handlers"
  liftIO $ installHandler sigINT (handler loggingLevel shutdownFlag threadIds "sigINT") Nothing
  liftIO $ installHandler sigTERM (handler loggingLevel shutdownFlag threadIds "sigTERM") Nothing
  logInfo _CMP_WORKER "handlers installed"

handler loggingLevel shutdownFlag threadIds typeSignal =
  CatchOnce . runLogging loggingLevel $ do
    logInfo _CMP_WORKER "shuting down workers: started"
    liftIO $ traverse killThread threadIds
    logInfo _CMP_WORKER "shuting down workers: notifing web server"
    liftIO $ putMVar shutdownFlag ()
    logInfo _CMP_WORKER "shuting down workers: completed"
