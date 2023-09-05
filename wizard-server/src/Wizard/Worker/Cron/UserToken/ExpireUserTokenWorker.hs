module Wizard.Worker.Cron.UserToken.ExpireUserTokenWorker (
  expireUserTokenWorker,
) where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import System.Cron
import Prelude hiding (log)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Logger
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.UserToken.ApiKey.ApiKeyService
import Wizard.Util.Context

expireUserTokenWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
expireUserTokenWorker context =
  when
    context.serverConfig.userToken.expire.enabled
    (addJob (job context) (T.pack $ context.serverConfig.userToken.expire.cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext expireApiKeys context
        log "ended"

-- -----------------------------------------------------------------------------
log msg = logInfo _CMP_WORKER ("ExpireUserTokenWorker: " ++ msg)
