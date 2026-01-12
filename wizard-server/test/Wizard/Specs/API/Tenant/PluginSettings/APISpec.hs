module Wizard.Specs.API.Tenant.PluginSettings.APISpec where

import Test.Hspec

import Wizard.Specs.API.Tenant.PluginSettings.Detail_GET
import Wizard.Specs.API.Tenant.PluginSettings.Detail_PUT
import Wizard.Specs.API.Tenant.PluginSettings.List_PUT

tenantPluginSettingsAPI appContext =
  describe "TENANT PLUGIN SETTINGS API Spec" $ do
    list_PUT appContext
    detail_GET appContext
    detail_PUT appContext
