module Wizard.Specs.API.Tenant.PluginSettings.APISpec where

import Test.Hspec

import Wizard.Specs.API.Tenant.PluginSettings.Detail_GET
import Wizard.Specs.API.Tenant.PluginSettings.Detail_PUT

tenantPluginSettingsAPI appContext =
  describe "TENANT PLUGIN SETTINGS API Spec" $ do
    detail_GET appContext
    detail_PUT appContext
