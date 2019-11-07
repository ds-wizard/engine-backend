module Database.Migration.Development.Organization.OrganizationMigration where

import Constant.Component
import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Development.Organization.Data.Organizations
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Organization/Organization) started"
  deleteOrganizations
  insertOrganization orgDsw
  insertOrganization orgNetherlands
  logInfo $ msg _CMP_MIGRATION "(Organization/Organization) ended"
