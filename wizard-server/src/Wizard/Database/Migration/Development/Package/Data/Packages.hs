module Wizard.Database.Migration.Development.Package.Data.Packages where

import Data.Maybe (fromJust)
import Data.Time

import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.Common.Database.Migration.Development.Organization.Data.Organizations
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

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
    , nonEditable = False
    , events = [AddKnowledgeModelEvent' a_km1]
    , appUuid = differentApp.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
