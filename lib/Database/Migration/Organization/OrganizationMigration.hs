module Database.Migration.Organization.OrganizationMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Organization.Data.Organizations

runMigration = do
  $(logInfo) "MIGRATION (Organization/Organization): started"
  deleteOrganizations
  insertOrganization org1
  $(logInfo) "MIGRATION (Organization/Organization): ended"
