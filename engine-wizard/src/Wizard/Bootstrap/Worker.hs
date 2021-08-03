module Wizard.Bootstrap.Worker
  ( worker
  ) where

import Control.Concurrent
import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import System.Cron
import System.Posix.Signals (Handler(CatchOnce), installHandler, sigINT, sigTERM)

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Util.Logger
import Wizard.Worker.Cron.Document.DocumentWorker
import Wizard.Worker.Cron.Feedback.FeedbackWorker
import Wizard.Worker.Cron.Questionnaire.CleanQuestionnaireWorker
import Wizard.Worker.Cron.Questionnaire.SquashQuestionnaireEventsWorker

worker :: MVar () -> BaseContext -> IO ()
worker shutdownFlag context = do
  permanentWorker shutdownFlag context
  cronJob shutdownFlag context

-- ------------------------------------------------------------------
permanentWorker :: MVar () -> BaseContext -> IO ()
permanentWorker _ _ = return ()

-- ------------------------------------------------------------------
cronJob :: MVar () -> BaseContext -> IO ()
cronJob shutdownFlag context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        logInfo _CMP_WORKER "scheduling workers started"
        threadIds <-
          liftIO . execSchedule $ do
            feedbackWorker context
            documentWorker context
            cleanQuestionnaireWorker context
            squashQuestionnaireEventsWorker context
        setupHandlers loggingLevel shutdownFlag threadIds
        logInfo _CMP_WORKER "scheduling workers completed"

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
