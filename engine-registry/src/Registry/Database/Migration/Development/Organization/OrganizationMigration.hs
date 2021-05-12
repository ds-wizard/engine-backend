module Registry.Database.Migration.Development.Organization.OrganizationMigration where

import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/Organization) started"
  insertOrganization orgGlobal
  insertOrganization orgNetherlands
  logInfo _CMP_MIGRATION "(Fixtures/Organization) ended"
