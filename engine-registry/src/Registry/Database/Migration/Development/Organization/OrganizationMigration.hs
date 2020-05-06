module Registry.Database.Migration.Development.Organization.OrganizationMigration where

import Registry.Constant.Component
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Organization/Organization) started"
  deleteOrganizations
  insertOrganization orgGlobal
  insertOrganization orgNetherlands
  logInfo _CMP_MIGRATION "(Organization/Organization) ended"
