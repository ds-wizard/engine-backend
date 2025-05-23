module Wizard.Service.Package.Publish.PackagePublishMapper where

import Data.Time

import Wizard.Model.Branch.Branch
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.Public.Model.Tenant.Config.TenantConfig

fromPackage
  :: Branch
  -> Maybe String
  -> Maybe String
  -> TenantConfigOrganization
  -> String
  -> String
  -> String
  -> [Event]
  -> UTCTime
  -> PackageWithEvents
fromPackage branch forkOfPkgId mergeCheckpointPkgId org version description readme events now =
  PackageWithEvents
    { pId = buildCoordinate org.organizationId branch.kmId version
    , name = branch.name
    , organizationId = org.organizationId
    , kmId = branch.kmId
    , version = version
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = description
    , readme = readme
    , license = branch.license
    , previousPackageId = branch.previousPackageId
    , forkOfPackageId = forkOfPkgId
    , mergeCheckpointPackageId = mergeCheckpointPkgId
    , events = events
    , nonEditable = False
    , tenantUuid = branch.tenantUuid
    , createdAt = now
    }
