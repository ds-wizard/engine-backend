module Wizard.Database.Migration.Development.ActionKey.ActionKeyMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(ActionKey/ActionKey) started"
  deleteActionKeys
  insertActionKey differentActionKey
  logInfo _CMP_MIGRATION "(ActionKey/ActionKey) ended"
