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
        { _appContextAppConfig = baseContext ^. appConfig
        , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
        , _appContextPool = baseContext ^. pool
        , _appContextTraceUuid = traceUuid
        , _appContextCurrentOrganization = Nothing
        }
  runReaderT (runAppContextM function) appContext

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetCurrentOrganization callback = do
  mCurrentOrganization <- asks _appContextCurrentOrganization
  case mCurrentOrganization of
    Just user -> callback user
    Nothing -> return . Left . createErrorWithErrorMessage $ _ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION

hmGetCurrentOrganization callback = do
  mCurrentOrganization <- asks _appContextCurrentOrganization
  case mCurrentOrganization of
    Just user -> callback user
    Nothing -> return . Just . createErrorWithErrorMessage $ _ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION
