module Wizard.Service.Version.VersionMapper where

import Control.Lens ((&), (.~), (^.))
import Data.Time

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Model.Branch.Branch
import Wizard.Service.Package.PackageUtils

fromBranch :: BranchWithEvents -> PackageWithEvents -> BranchWithEvents
fromBranch branch pkg = (branch & events .~ []) & previousPackageId .~ (Just $ pkg ^. pId)

fromPackage ::
     BranchWithEvents
  -> VersionDTO
  -> Maybe String
  -> Maybe String
  -> OrganizationDTO
  -> String
  -> [Event]
  -> UTCTime
  -> PackageWithEvents
fromPackage branch versionDto forkOfPkgId mergeCheckpointPkgId org version events now =
  PackageWithEvents
    { _packageWithEventsPId = buildPackageId (org ^. organizationId) (branch ^. kmId) version
    , _packageWithEventsName = branch ^. name
    , _packageWithEventsOrganizationId = org ^. organizationId
    , _packageWithEventsKmId = branch ^. kmId
    , _packageWithEventsVersion = version
    , _packageWithEventsMetamodelVersion = kmMetamodelVersion
    , _packageWithEventsDescription = versionDto ^. description
    , _packageWithEventsReadme = versionDto ^. readme
    , _packageWithEventsLicense = versionDto ^. license
    , _packageWithEventsPreviousPackageId = branch ^. previousPackageId
    , _packageWithEventsForkOfPackageId = forkOfPkgId
    , _packageWithEventsMergeCheckpointPackageId = mergeCheckpointPkgId
    , _packageWithEventsEvents = events
    , _packageWithEventsCreatedAt = now
    }
