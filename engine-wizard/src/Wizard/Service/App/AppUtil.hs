module Wizard.Service.App.AppUtil where

import Control.Lens ((^.))

import LensesConfig hiding (hash)
import Wizard.Api.Resource.App.AppDTO
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppMapper
import Wizard.Service.Config.AppConfigService

enhanceApp :: App -> AppContextM AppDTO
enhanceApp app = do
  appConfig <- getAppConfig
  return $ toDTO app (appConfig ^. lookAndFeel . logoUrl) (appConfig ^. lookAndFeel . primaryColor)
