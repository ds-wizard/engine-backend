module Wizard.Service.App.AppUtil where

import Wizard.Api.Resource.App.AppDTO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppMapper
import Wizard.Service.Config.AppConfigService

enhanceApp :: App -> AppContextM AppDTO
enhanceApp app = do
  appConfig <- getAppConfigByUuid app.uuid
  return $ toDTO app appConfig.lookAndFeel.logoUrl appConfig.lookAndFeel.primaryColor
