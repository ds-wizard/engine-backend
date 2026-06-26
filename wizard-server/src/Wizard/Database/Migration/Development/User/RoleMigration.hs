module Wizard.Database.Migration.Development.User.RoleMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.Migration.Development.User.Data.Roles
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.User.RoleDAO

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Role/Role) started"
  deleteRoles
  insertRole adminRole
  insertRole dataStewardRole
  insertRole researcherRole
  insertRole differentAdminRole
  insertRole differentDataStewardRole
  insertRole differentResearcherRole
  logInfo _CMP_MIGRATION "(Role/Role) ended"
