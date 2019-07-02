module Service.Package.PackageMapper where

import Control.Lens ((^.))
import Data.Time

import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Version.VersionDTO
import Constant.KnowledgeModel
import Integration.Resource.Package.PackageSimpleIDTO
import LensesConfig
import Model.Branch.Branch
import Model.Event.Event
import Model.Package.Package
import Model.Package.PackageWithEvents
import Service.Event.EventMapper
import Service.Organization.OrganizationMapper
import Service.Package.PackageUtils

toPackage :: PackageWithEvents -> Package
toPackage pkg =
  Package
  { _packagePId = pkg ^. pId
  , _packageName = pkg ^. name
  , _packageOrganizationId = pkg ^. organizationId
  , _packageKmId = pkg ^. kmId
  , _packageVersion = pkg ^. version
  , _packageMetamodelVersion = pkg ^. metamodelVersion
  , _packageDescription = pkg ^. description
  , _packageReadme = pkg ^. readme
  , _packageParentPackageId = pkg ^. parentPackageId
  , _packageCreatedAt = pkg ^. createdAt
  }

toDTO :: PackageWithEvents -> PackageDTO
toDTO pkg =
  PackageDTO
  { _packageDTOPId = pkg ^. pId
  , _packageDTOName = pkg ^. name
  , _packageDTOOrganizationId = pkg ^. organizationId
  , _packageDTOKmId = pkg ^. kmId
  , _packageDTOVersion = pkg ^. version
  , _packageDTOMetamodelVersion = pkg ^. metamodelVersion
  , _packageDTODescription = pkg ^. description
  , _packageDTOReadme = pkg ^. readme
  , _packageDTOParentPackageId = pkg ^. parentPackageId
  , _packageDTOEvents = toDTOs (pkg ^. events)
  , _packageDTOCreatedAt = pkg ^. createdAt
  }

toSimpleDTO :: Package -> PackageSimpleDTO
toSimpleDTO pkg = toSimpleDTO' pkg [] []

toSimpleDTO' :: Package -> [PackageSimpleIDTO] -> [String] -> PackageSimpleDTO
toSimpleDTO' pkg pkgRs localVersions =
  PackageSimpleDTO
  { _packageSimpleDTOPId = pkg ^. pId
  , _packageSimpleDTOName = pkg ^. name
  , _packageSimpleDTOOrganizationId = pkg ^. organizationId
  , _packageSimpleDTOKmId = pkg ^. kmId
  , _packageSimpleDTOVersion = pkg ^. version
  , _packageSimpleDTOVersions = localVersions
  , _packageSimpleDTODescription = pkg ^. description
  , _packageSimpleDTOState = computePackageState pkgRs pkg
  , _packageSimpleDTOOrganization =
      case selectPackageByOrgIdAndKmId pkg pkgRs of
        Just pkgR -> Just . fromSimpleIntegration $ pkgR ^. organization
        Nothing -> Nothing
  , _packageSimpleDTOCreatedAt = pkg ^. createdAt
  }

toDetailDTO :: Package -> [PackageSimpleIDTO] -> [String] -> String -> PackageDetailDTO
toDetailDTO pkg pkgRs versionLs registryLink =
  PackageDetailDTO
  { _packageDetailDTOPId = pkg ^. pId
  , _packageDetailDTOName = pkg ^. name
  , _packageDetailDTOOrganizationId = pkg ^. organizationId
  , _packageDetailDTOKmId = pkg ^. kmId
  , _packageDetailDTOVersion = pkg ^. version
  , _packageDetailDTODescription = pkg ^. description
  , _packageDetailDTOReadme = pkg ^. readme
  , _packageDetailDTOMetamodelVersion = pkg ^. metamodelVersion
  , _packageDetailDTOParentPackageId = pkg ^. parentPackageId
  , _packageDetailDTOVersions = versionLs
  , _packageDetailDTORemoteLatestVersion =
      case selectPackageByOrgIdAndKmId pkg pkgRs of
        Just pkgR -> Just $ pkgR ^. version
        Nothing -> Nothing
  , _packageDetailDTOState = computePackageState pkgRs pkg
  , _packageDetailDTORegistryLink =
      case selectPackageByOrgIdAndKmId pkg pkgRs of
        Just pkgR -> Just registryLink
        Nothing -> Nothing
  , _packageDetailDTOOrganization =
      case selectPackageByOrgIdAndKmId pkg pkgRs of
        Just pkgR -> Just . fromSimpleIntegration $ pkgR ^. organization
        Nothing -> Nothing
  , _packageDetailDTOCreatedAt = pkg ^. createdAt
  }

fromDTO :: PackageDTO -> PackageWithEvents
fromDTO dto =
  PackageWithEvents
  { _packageWithEventsPId = dto ^. pId
  , _packageWithEventsName = dto ^. name
  , _packageWithEventsOrganizationId = dto ^. organizationId
  , _packageWithEventsKmId = dto ^. kmId
  , _packageWithEventsVersion = dto ^. version
  , _packageWithEventsMetamodelVersion = dto ^. metamodelVersion
  , _packageWithEventsDescription = dto ^. description
  , _packageWithEventsReadme = dto ^. readme
  , _packageWithEventsParentPackageId = dto ^. parentPackageId
  , _packageWithEventsEvents = fromDTOs (dto ^. events)
  , _packageWithEventsCreatedAt = dto ^. createdAt
  }

fromBranchAndVersion ::
     BranchWithEvents -> VersionDTO -> OrganizationDTO -> String -> [Event] -> UTCTime -> PackageWithEvents
fromBranchAndVersion branch versionDto organization version events now =
  PackageWithEvents
  { _packageWithEventsPId = buildPackageId (organization ^. organizationId) (branch ^. kmId) version
  , _packageWithEventsName = branch ^. name
  , _packageWithEventsOrganizationId = organization ^. organizationId
  , _packageWithEventsKmId = branch ^. kmId
  , _packageWithEventsVersion = version
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = versionDto ^. description
  , _packageWithEventsReadme = versionDto ^. readme
  , _packageWithEventsParentPackageId = branch ^. parentPackageId
  , _packageWithEventsEvents = events
  , _packageWithEventsCreatedAt = now
  }

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgKmId pkgVersion = pkgOrganizationId ++ ":" ++ pkgKmId ++ ":" ++ pkgVersion

buildPackageUrl :: String -> String -> String
buildPackageUrl clientRegistryUrl pkgId = clientRegistryUrl ++ "/knowledge-models/" ++ pkgId
