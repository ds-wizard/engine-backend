module Wizard.Specs.API.Tenant.PluginSettings.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import Wizard.Model.Tenant.PluginSettings.TenantPluginSettings

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTenantPluginSettingsInDB appContext tenantPluginSettings = do
  tenantPluginSettingsFromDb <- getOneFromDB (findTenantPluginSettingsByPluginUuid tenantPluginSettings.pluginUuid) appContext
  liftIO $ tenantPluginSettingsFromDb.values `shouldBe` tenantPluginSettings.values
