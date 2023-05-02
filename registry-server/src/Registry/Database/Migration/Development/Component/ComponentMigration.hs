module Registry.Database.Migration.Development.Component.ComponentMigration where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger
import Shared.Common.Database.DAO.Component.ComponentDAO
import Shared.Common.Database.Migration.Development.Component.Data.Components

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(App/Component) started"
  deleteComponents
  insertComponent mailComponent
  logInfo _CMP_MIGRATION "(App/Component) ended"
