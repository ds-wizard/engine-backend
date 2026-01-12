module Wizard.Specs.API.User.PluginSettings.APISpec where

import Test.Hspec

import Wizard.Specs.API.User.PluginSettings.Detail_GET
import Wizard.Specs.API.User.PluginSettings.Detail_PUT

userPluginSettingsAPI appContext =
  describe "USER PLUGIN SETTINGS API Spec" $ do
    detail_GET appContext
    detail_PUT appContext
