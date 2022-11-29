module Wizard.Service.Version.VersionMapper where

import Data.Time

import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Coordinate
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Config.AppConfig

fromPackage
  :: Branch
  -> VersionDTO
  -> Maybe String
  -> Maybe String
  -> AppConfigOrganization
  -> String
  -> [Event]
  -> UTCTime
  -> PackageWithEvents
fromPackage branch versionDto forkOfPkgId mergeCheckpointPkgId org version events now =
  PackageWithEvents
    { pId = buildCoordinate org.organizationId branch.kmId version
    , name = branch.name
    , organizationId = org.organizationId
    , kmId = branch.kmId
    , version = version
    , metamodelVersion = kmMetamodelVersion
    , description = versionDto.description
    , readme = versionDto.readme
    , license = versionDto.license
    , previousPackageId = branch.previousPackageId
    , forkOfPackageId = forkOfPkgId
    , mergeCheckpointPackageId = mergeCheckpointPkgId
    , events = events
    , appUuid = branch.appUuid
    , createdAt = now
    }
