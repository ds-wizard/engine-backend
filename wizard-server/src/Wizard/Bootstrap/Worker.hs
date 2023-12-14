module Wizard.Bootstrap.Worker where

import Control.Concurrent

import Shared.Worker.Bootstrap.Worker
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Util.Context
import Wizard.Worker.Cron.Workers
import Wizard.Worker.Permanent.Workers

runWorker :: MVar () -> BaseContext -> IO ()
runWorker shutdownFlag context =
  worker runAppContextWithBaseContext runAppContextWithBaseContext'' shutdownFlag context workers permanentWorker
