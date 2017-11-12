module Database.Migration.Organization.OrganizationMigration where

import Data.Maybe
import qualified Data.UUID as U

import Context
import Database.DAO.Organization.OrganizationDAO
import Model.Organization.Organization
import Service.Organization.OrganizationService

runMigration context dspConfig logState = do
  logState "MIGRATION (Organization/Organization): started"
  deleteOrganizations context
  insertOrganization
    context
    Organization
    { _orgUuid =
        (fromJust (U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"))
    , _orgName = "Elixir Amsterdam"
    , _orgGroupId = "elixir.nl.amsterdam"
    }
  logState "MIGRATION (Organization/Organization): ended"
