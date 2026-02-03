module Wizard.Specs.API.User.PluginSettings.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.DAO.User.UserPluginSettingsDAO
import Wizard.Model.User.UserPluginSettings

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserPluginSettingsInDB appContext userPluginSettings = do
  userPluginSettingsFromDb <- getOneFromDB (findUserPluginSettingsByUserUuidAndPluginUuid userPluginSettings.userUuid userPluginSettings.pluginUuid) appContext
  liftIO $ userPluginSettingsFromDb.values `shouldBe` userPluginSettings.values
