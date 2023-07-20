module Wizard.Database.Migration.Development.ActionKey.ActionKeyMigration where

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(ActionKey/ActionKey) started"
  deleteActionKeys
  insertActionKey differentActionKey
  logInfo _CMP_MIGRATION "(ActionKey/ActionKey) ended"
