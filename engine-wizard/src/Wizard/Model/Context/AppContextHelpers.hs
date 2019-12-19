module Wizard.Model.Context.AppContextHelpers where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO, runReaderT)

import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.LensesConfig
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _appContextApplicationConfig = baseContext ^. appConfig
          , _appContextLocalization = baseContext ^. localization
          , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
          , _appContextPool = baseContext ^. pool
          , _appContextMsgChannel = baseContext ^. msgChannel
          , _appContextHttpClientManager = baseContext ^. httpClientManager
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentUser = Nothing
          }
  runReaderT (runAppContextM function) appContext

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetCurrentUser callback = do
  mCurrentUser <- asks _appContextCurrentUser
  case mCurrentUser of
    Just user -> callback user
    Nothing -> return . Left . UserError $ _ERROR_SERVICE_USER__MISSING_USER

hmGetCurrentUser callback = do
  mCurrentUser <- asks _appContextCurrentUser
  case mCurrentUser of
    Just user -> callback user
    Nothing -> return . Just . UserError $ _ERROR_SERVICE_USER__MISSING_USER
