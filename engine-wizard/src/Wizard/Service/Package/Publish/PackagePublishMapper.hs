module Wizard.Service.Package.Publish.PackagePublishMapper where

import Data.Time

import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Coordinate
import Wizard.Model.Branch.Branch
import Wizard.Model.Config.AppConfig

fromPackage
  :: Branch
  -> Maybe String
  -> Maybe String
  -> AppConfigOrganization
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
    , appUuid = branch.appUuid
    , createdAt = now
    }
