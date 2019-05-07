module Model.Context.AppContextHelpers where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO, runReaderT)

import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.ErrorHelpers
import Util.Uuid

runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
        { _appContextConfig = baseContext ^. config
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
    Nothing -> return . Left . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__MISSING_USER

hmGetCurrentUser callback = do
  mCurrentUser <- asks _appContextCurrentUser
  case mCurrentUser of
    Just user -> callback user
    Nothing -> return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__MISSING_USER
