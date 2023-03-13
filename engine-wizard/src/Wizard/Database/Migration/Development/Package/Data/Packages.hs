module Wizard.Database.Migration.Development.Package.Data.Packages where

import Data.Maybe (fromJust)
import Data.Time

import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App

globalRemotePackage :: PackageSimpleDTO
globalRemotePackage =
  PackageSimpleDTO
    { pId = globalPackage.pId
    , name = globalPackage.name
    , organizationId = globalPackage.organizationId
    , kmId = globalPackage.kmId
    , version = globalPackage.version
    , description = globalPackage.description
    , organization =
        OrganizationSimpleDTO
          { organizationId = orgGlobalSimple.organizationId
          , name = orgGlobalSimple.name
          , logo = Just orgLogo
          }
    , createdAt = globalPackage.createdAt
    }

globalNetherlandsPackage :: PackageSimpleDTO
globalNetherlandsPackage =
  PackageSimpleDTO
    { pId = netherlandsPackageV2.pId
    , name = netherlandsPackageV2.name
    , organizationId = netherlandsPackageV2.organizationId
    , kmId = netherlandsPackageV2.kmId
    , version = netherlandsPackageV2.version
    , description = netherlandsPackageV2.description
    , organization =
        OrganizationSimpleDTO
          { organizationId = orgNetherlandsSimple.organizationId
          , name = orgNetherlandsSimple.name
          , logo = Just orgLogo
          }
    , createdAt = globalPackage.createdAt
    }

differentPackage :: PackageWithEvents
differentPackage =
  PackageWithEvents
    { pId = "global:different:1.0.0"
    , name = "Different Knowledge Model"
    , organizationId = "global"
    , kmId = "different"
    , version = "1.0.0"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "Empty package"
    , readme = "# Different Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , events = [AddKnowledgeModelEvent' a_km1]
    , appUuid = differentApp.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
