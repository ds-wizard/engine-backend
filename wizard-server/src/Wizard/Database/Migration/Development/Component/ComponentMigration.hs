module Wizard.Database.Migration.Development.Component.ComponentMigration where

import Shared.Common.Database.DAO.Component.ComponentDAO
import Shared.Common.Database.Migration.Development.Component.Data.Components
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(App/Component) started"
  deleteComponents
  insertComponent mailComponent
  logInfo _CMP_MIGRATION "(App/Component) ended"
