module Wizard.Service.Config.ClientConfigService where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Config.ClientConfigMapper

getClientConfig :: Maybe String -> AppContextM ClientConfigDTO
getClientConfig mClientUrl = do
  serverConfig <- asks _appContextServerConfig
  app <-
    if serverConfig ^. cloud . enabled
      then maybe getCurrentApp findAppByClientUrl mClientUrl
      else getCurrentApp
  unless (app ^. enabled) (throwError LockedError)
  appConfig <-
    if serverConfig ^. cloud . enabled
      then case mClientUrl of
             Just clientUrl -> getAppConfigByUuid (app ^. uuid)
             Nothing -> getAppConfig
      else getAppConfig
  locales <- findLocales
  return $ toClientConfigDTO serverConfig appConfig app locales
