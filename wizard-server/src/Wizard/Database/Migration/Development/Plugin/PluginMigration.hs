module Wizard.Database.Migration.Development.Plugin.PluginMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Plugin.PluginDAO
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Plugin/Plugin) started"
  deletePlugins
  insertPlugin plugin1
  insertPlugin differentPlugin1
  logInfo _CMP_MIGRATION "(Plugin/Plugin) ended"
