module Wizard.Worker.Cron.UserToken.CleanUserTokenWorker
  ( cleanUserTokenWorker
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T
import Prelude hiding (log)
import System.Cron

import LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.UserTokenService
import Wizard.Util.Context
import Wizard.Util.Logger

cleanUserTokenWorker :: (MonadSchedule m, Applicative m) => BaseContext -> m ()
cleanUserTokenWorker context =
  when
    (context ^. serverConfig . userToken . clean . enabled)
    (addJob (job context) (T.pack $ context ^. serverConfig . userToken . clean . cron))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
job :: BaseContext -> IO ()
job context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel $ do
        log "starting"
        liftIO $ runAppContextWithBaseContext cleanTokens context
        log "ended"

-- -----------------------------------------------------------------------------
log msg = logInfo _CMP_WORKER ("CleanUserTokenWorker: " ++ msg)
