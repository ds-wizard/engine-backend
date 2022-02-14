module Wizard.Service.Config.ClientConfigService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)

import LensesConfig
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppService
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Config.ClientConfigMapper

getClientConfig :: Maybe String -> AppContextM ClientConfigDTO
getClientConfig mClientUrl =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    app <-
      if serverConfig ^. cloud . enabled
        then maybe getCurrentApp findAppByClientUrl mClientUrl
        else getCurrentApp
    appConfig <-
      if serverConfig ^. cloud . enabled
        then case mClientUrl of
               Just clientUrl -> getAppConfigByUuid (app ^. uuid)
               Nothing -> getAppConfig
        else getAppConfig
    return $ toClientConfigDTO serverConfig appConfig app
