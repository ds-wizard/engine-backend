module Wizard.Worker.Permanent.Workers where

import Control.Concurrent
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)

import Wizard.Model.Context.BaseContext
import Wizard.Worker.Permanent.PersistentCommand.PersistentCommandListenerWorker

permanentWorker :: BaseContext -> LoggingT IO [ThreadId]
permanentWorker context = do
  threadId <- liftIO $ forkIO (persistentCommandListenerJob context)
  return [threadId]
