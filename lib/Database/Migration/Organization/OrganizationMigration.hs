module Database.Migration.Organization.OrganizationMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Organization.Data.Organizations
import LensesConfig
import Model.Context.AppContext
import Model.Organization.Organization
import Service.Organization.OrganizationService

runMigration appContext = do
  $(logInfo) "MIGRATION (Organization/Organization): started"
  let context = appContext ^. oldContext
  liftIO $ deleteOrganizations context
  liftIO $ insertOrganization context org1
  $(logInfo) "MIGRATION (Organization/Organization): ended"
