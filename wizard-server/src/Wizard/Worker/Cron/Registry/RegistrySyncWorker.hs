module Wizard.Worker.Cron.Registry.RegistrySyncWorker (
  registrySyncWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Registry.RegistryService
import Wizard.Util.Context
import Wizard.Util.Logger

registrySyncWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
registrySyncWorker context =
  when
    context.serverConfig.registry.sync.enabled
    (addJob (job context) (T.pack $ context.serverConfig.registry.sync.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext synchronizeData context
        log "ended"

-- -----------------------------------------------------------------------------
log msg = logInfo _CMP_WORKER ("RegistryWorker: " ++ msg)
