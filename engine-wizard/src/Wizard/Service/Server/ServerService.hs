module Wizard.Service.Server.ServerService where

import Control.Concurrent.MVar
import Control.Monad.Reader (asks, liftIO)

import Wizard.Model.Context.AppContext

restartServer :: AppContextM ()
restartServer = do
  shutdownFlag <- asks _appContextShutdownFlag
  liftIO $ putMVar shutdownFlag ()
