module Registry.Worker.Cron.PersistentCommand.PersistentCommandRetryWorker (
  persistentCommandRetryWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron hiding (cron)
import Prelude hiding (log)

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Registry.Service.PersistentCommand.PersistentCommandService
import Registry.Util.Context
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger

persistentCommandRetryWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
persistentCommandRetryWorker context =
  when
    context.serverConfig.persistentCommand.retryJob.enabled
    (addJob (job context) (T.pack $ context.serverConfig.persistentCommand.retryJob.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext runPersistentCommands' context
        log "ended"

log msg = logInfo _CMP_WORKER ("PersistentCommandRetryWorker: " ++ msg)
