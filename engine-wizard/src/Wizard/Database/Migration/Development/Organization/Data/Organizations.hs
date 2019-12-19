module Wizard.Database.Migration.Development.Organization.Data.Organizations where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.LensesConfig
import Wizard.Model.Organization.Organization

org1 :: Organization
org1 =
  Organization
    { _organizationUuid = fromJust $ U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
    , _organizationName = "Organization Amsterdam"
    , _organizationOrganizationId = "org.nl.amsterdam"
    , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

editedOrg1 :: Organization
editedOrg1 =
  Organization
    { _organizationUuid = org1 ^. uuid
    , _organizationName = "EDITED: Organization Leiden"
    , _organizationOrganizationId = "org.nl.leiden"
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

orgGlobal :: Organization
orgGlobal =
  Organization
    { _organizationUuid = fromJust $ U.fromString "c30e30d2-a033-4c97-b683-bc74f78937c3"
    , _organizationName = "Organization"
    , _organizationOrganizationId = "global"
    , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgNetherlands :: Organization
orgNetherlands =
  Organization
    { _organizationUuid = fromJust $ U.fromString "f89f390a-b6af-4935-8dbe-00e1fd4955be"
    , _organizationName = "Organization Netherlands"
    , _organizationOrganizationId = "org.nl"
    , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
