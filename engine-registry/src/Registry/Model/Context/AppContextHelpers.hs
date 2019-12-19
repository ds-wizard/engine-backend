module Registry.Model.Context.AppContextHelpers where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO, runReaderT)

import Registry.LensesConfig
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Shared.Model.Error.Error
import Shared.Util.Uuid

runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _appContextApplicationConfig = baseContext ^. appConfig
          , _appContextLocalization = baseContext ^. localization
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
    Nothing -> return . Left . UserError $ _ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION

hmGetCurrentOrganization callback = do
  mCurrentOrganization <- asks _appContextCurrentOrganization
  case mCurrentOrganization of
    Just user -> callback user
    Nothing -> return . Just . UserError $ _ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION
