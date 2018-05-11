module Database.Migration.Organization.OrganizationMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)

import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Organization.Data.Organizations
import LensesConfig

runMigration appContext = do
  $(logInfo) "MIGRATION (Organization/Organization): started"
  let context = appContext ^. oldContext
  liftIO $ deleteOrganizations context
  liftIO $ insertOrganization context org1
  $(logInfo) "MIGRATION (Organization/Organization): ended"
