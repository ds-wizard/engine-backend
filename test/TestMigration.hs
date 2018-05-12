module TestMigration where

import Control.Lens ((^.))

import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.User.UserDAO
import Database.Migration.Organization.Data.Organizations
import Database.Migration.User.Data.Users
import LensesConfig

resetDB appContext = do
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  deleteUsers context
  insertUser context userAlbert
  deleteOrganizations context
  insertOrganization context org1
  deleteBranches context
  deleteMigratorStates context
  deleteActionKeys context
  return ()
