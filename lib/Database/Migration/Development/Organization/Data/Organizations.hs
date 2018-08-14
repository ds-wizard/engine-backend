module Database.Migration.Development.Organization.Data.Organizations where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Model.Organization.Organization

org1 =
  Organization
  { _organizationUuid = fromJust $ U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
  , _organizationName = "Elixir Amsterdam"
  , _organizationOrganizationId = "elixir.nl.amsterdam"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }
