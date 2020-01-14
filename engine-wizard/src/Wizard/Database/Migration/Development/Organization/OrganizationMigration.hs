module Wizard.Database.Migration.Development.Organization.OrganizationMigration where

import Wizard.Constant.Component
import Wizard.Database.DAO.Organization.OrganizationDAO
import Wizard.Database.Migration.Development.Organization.Data.Organizations
import Wizard.Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Organization/Organization) started"
  deleteOrganizations
  insertOrganization org1
  logInfo $ msg _CMP_MIGRATION "(Organization/Organization) ended"
