module Database.Migration.Development.Organization.Data.Organizations where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Organization.OrganizationChangeDTO
import LensesConfig
import Model.Organization.Organization

org1 :: Organization
org1 =
  Organization
  { _organizationUuid = fromJust $ U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
  , _organizationName = "DSW Amsterdam"
  , _organizationOrganizationId = "dsw.nl.amsterdam"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

editedOrg1 :: Organization
editedOrg1 =
  Organization
  { _organizationUuid = org1 ^. uuid
  , _organizationName = "EDITED: DSW Leiden"
  , _organizationOrganizationId = "dsw.nl.leiden"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
  }

editedOrg1Change :: OrganizationChangeDTO
editedOrg1Change =
  OrganizationChangeDTO
  { _organizationChangeDTOUuid = editedOrg1 ^. uuid
  , _organizationChangeDTOName = editedOrg1 ^. name
  , _organizationChangeDTOOrganizationId = editedOrg1 ^. organizationId
  }

orgDsw :: Organization
orgDsw =
  Organization
  { _organizationUuid = fromJust $ U.fromString "c30e30d2-a033-4c97-b683-bc74f78937c3"
  , _organizationName = "DSW"
  , _organizationOrganizationId = "dsw"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

orgNetherlands :: Organization
orgNetherlands =
  Organization
  { _organizationUuid = fromJust $ U.fromString "f89f390a-b6af-4935-8dbe-00e1fd4955be"
  , _organizationName = "DSW Netherlands"
  , _organizationOrganizationId = "dsw.nl"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }
