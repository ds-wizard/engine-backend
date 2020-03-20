module Wizard.Service.Package.PackageMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Event.EventMapper
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Service.Organization.OrganizationMapper
import Wizard.Service.Package.PackageUtils

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
    , _packageDetailDTOLicense = pkg ^. license
    , _packageDetailDTOMetamodelVersion = pkg ^. metamodelVersion
    , _packageDetailDTOPreviousPackageId = pkg ^. previousPackageId
    , _packageDetailDTOForkOfPackageId = pkg ^. forkOfPackageId
    , _packageDetailDTOMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
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
    , _packageWithEventsLicense = dto ^. license
    , _packageWithEventsPreviousPackageId = dto ^. previousPackageId
    , _packageWithEventsForkOfPackageId = dto ^. forkOfPackageId
    , _packageWithEventsMergeCheckpointPackageId = dto ^. mergeCheckpointPackageId
    , _packageWithEventsEvents = fromDTOs (dto ^. events)
    , _packageWithEventsCreatedAt = dto ^. createdAt
    }

buildPackageUrl :: String -> String -> String
buildPackageUrl clientRegistryUrl pkgId = clientRegistryUrl ++ "/knowledge-models/" ++ pkgId
