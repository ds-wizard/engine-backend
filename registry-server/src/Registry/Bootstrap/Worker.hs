module Registry.Bootstrap.Worker where

import Control.Concurrent

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Registry.Util.Context
import Registry.Worker.Cron.Workers
import Registry.Worker.Permanent.Workers
import Shared.Worker.Bootstrap.Worker

runWorker :: MVar () -> BaseContext -> IO ()
runWorker shutdownFlag context =
  worker runAppContextWithBaseContext runAppContextWithBaseContext'' shutdownFlag context workers permanentWorker
