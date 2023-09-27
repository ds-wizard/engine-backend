module Shared.Component.Database.Migration.Development.Component.ComponentMigration where

import Shared.Common.Constant.Component
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Component.Database.DAO.Component.ComponentDAO
import Shared.Component.Database.Migration.Development.Component.Data.Components

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Component/Component) started"
  deleteComponents
  insertComponent mailComponent
  logInfo _CMP_MIGRATION "(Component/Component) ended"
