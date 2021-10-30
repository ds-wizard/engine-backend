module Wizard.Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Wizard.Database.Migration.Development.App.Data.Apps

globalRemotePackage :: PackageSimpleDTO
globalRemotePackage =
  PackageSimpleDTO
    { _packageSimpleDTOPId = globalPackage ^. pId
    , _packageSimpleDTOName = globalPackage ^. name
    , _packageSimpleDTOOrganizationId = globalPackage ^. organizationId
    , _packageSimpleDTOKmId = globalPackage ^. kmId
    , _packageSimpleDTOVersion = globalPackage ^. version
    , _packageSimpleDTODescription = globalPackage ^. description
    , _packageSimpleDTOOrganization =
        OrganizationSimpleDTO
          { _organizationSimpleDTOOrganizationId = orgGlobalSimple ^. organizationId
          , _organizationSimpleDTOName = orgGlobalSimple ^. name
          , _organizationSimpleDTOLogo = Just orgLogo
          }
    , _packageSimpleDTOCreatedAt = globalPackage ^. createdAt
    }

globalNetherlandsPackage :: PackageSimpleDTO
globalNetherlandsPackage =
  PackageSimpleDTO
    { _packageSimpleDTOPId = netherlandsPackageV2 ^. pId
    , _packageSimpleDTOName = netherlandsPackageV2 ^. name
    , _packageSimpleDTOOrganizationId = netherlandsPackageV2 ^. organizationId
    , _packageSimpleDTOKmId = netherlandsPackageV2 ^. kmId
    , _packageSimpleDTOVersion = netherlandsPackageV2 ^. version
    , _packageSimpleDTODescription = netherlandsPackageV2 ^. description
    , _packageSimpleDTOOrganization =
        OrganizationSimpleDTO
          { _organizationSimpleDTOOrganizationId = orgNetherlandsSimple ^. organizationId
          , _organizationSimpleDTOName = orgNetherlandsSimple ^. name
          , _organizationSimpleDTOLogo = Just orgLogo
          }
    , _packageSimpleDTOCreatedAt = globalPackage ^. createdAt
    }

differentPackage :: PackageWithEvents
differentPackage =
  PackageWithEvents
    { _packageWithEventsPId = "global:different:1.0.0"
    , _packageWithEventsName = "Different Knowledge Model"
    , _packageWithEventsOrganizationId = "global"
    , _packageWithEventsKmId = "different"
    , _packageWithEventsVersion = "1.0.0"
    , _packageWithEventsMetamodelVersion = kmMetamodelVersion
    , _packageWithEventsDescription = "Empty package"
    , _packageWithEventsReadme = "# Different Knowledge Model"
    , _packageWithEventsLicense = "Apache-2.0"
    , _packageWithEventsPreviousPackageId = Nothing
    , _packageWithEventsForkOfPackageId = Nothing
    , _packageWithEventsMergeCheckpointPackageId = Nothing
    , _packageWithEventsEvents = [AddKnowledgeModelEvent' a_km1]
    , _packageWithEventsAppUuid = differentApp ^. uuid
    , _packageWithEventsCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
