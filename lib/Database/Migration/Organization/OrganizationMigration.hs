module Database.Migration.Organization.OrganizationMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Database.DAO.Organization.OrganizationDAO
import LensesConfig
import Model.Context.AppContext
import Model.Organization.Organization
import Service.Organization.OrganizationService

runMigration appContext = do
  $(logInfo) "MIGRATION (Organization/Organization): started"
  let context = appContext ^. oldContext
  liftIO $ deleteOrganizations context
  liftIO $
    insertOrganization
      context
      Organization
      { _orgUuid = (fromJust (U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"))
      , _orgName = "Elixir Amsterdam"
      , _orgGroupId = "elixir.nl.amsterdam"
      }
  $(logInfo) "MIGRATION (Organization/Organization): ended"
