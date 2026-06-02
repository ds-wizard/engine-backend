module Wizard.Database.Migration.Development.UserEmailLink.UserEmailLinkMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Wizard.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Wizard.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(UserEmailLink/UserEmailLink) started"
  deleteUserEmailLinks
  insertUserEmailLink differentUserEmailLink
  logInfo _CMP_MIGRATION "(UserEmailLink/UserEmailLink) ended"
