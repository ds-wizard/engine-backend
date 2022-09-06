module Wizard.Service.Package.PackageMapper where

import Control.Lens ((^.))
import qualified Data.List as L
import qualified Data.UUID as U

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Coordinate
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Package.PackageList
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Service.Package.PackageUtil

toSimpleDTO :: Package -> PackageSimpleDTO
toSimpleDTO = toSimpleDTO' [] []

toSimpleDTO' :: [RegistryPackage] -> [RegistryOrganization] -> Package -> PackageSimpleDTO
toSimpleDTO' pkgRs orgRs pkg =
  PackageSimpleDTO
    { _packageSimpleDTOPId = pkg ^. pId
    , _packageSimpleDTOName = pkg ^. name
    , _packageSimpleDTOOrganizationId = pkg ^. organizationId
    , _packageSimpleDTOKmId = pkg ^. kmId
    , _packageSimpleDTOVersion = pkg ^. version
    , _packageSimpleDTORemoteLatestVersion =
        case selectPackageByOrgIdAndKmId pkg pkgRs of
          Just pkgR -> Just $ pkgR ^. remoteVersion
          Nothing -> Nothing
    , _packageSimpleDTODescription = pkg ^. description
    , _packageSimpleDTOState = computePackageState pkgRs pkg
    , _packageSimpleDTOOrganization = selectOrganizationByOrgId pkg orgRs
    , _packageSimpleDTOCreatedAt = pkg ^. createdAt
    }

toSimpleDTO'' :: PackageList -> PackageSimpleDTO
toSimpleDTO'' pkg =
  PackageSimpleDTO
    { _packageSimpleDTOPId = pkg ^. pId
    , _packageSimpleDTOName = pkg ^. name
    , _packageSimpleDTOOrganizationId = pkg ^. organizationId
    , _packageSimpleDTOKmId = pkg ^. kmId
    , _packageSimpleDTOVersion = pkg ^. version
    , _packageSimpleDTORemoteLatestVersion = pkg ^. remoteVersion
    , _packageSimpleDTODescription = pkg ^. description
    , _packageSimpleDTOState = computePackageState' pkg
    , _packageSimpleDTOOrganization =
        case pkg ^. remoteOrganizationName of
          Just orgName ->
            Just $
            RegistryOrganization
              { _registryOrganizationOrganizationId = pkg ^. organizationId
              , _registryOrganizationName = orgName
              , _registryOrganizationLogo = pkg ^. remoteOrganizationLogo
              , _registryOrganizationCreatedAt = pkg ^. createdAt
              }
          Nothing -> Nothing
    , _packageSimpleDTOCreatedAt = pkg ^. createdAt
    }

toDetailDTO :: Package -> [RegistryPackage] -> [RegistryOrganization] -> [String] -> Maybe String -> PackageDetailDTO
toDetailDTO pkg pkgRs orgRs versionLs registryLink =
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
    , _packageDetailDTOVersions = L.sort versionLs
    , _packageDetailDTORemoteLatestVersion =
        case selectPackageByOrgIdAndKmId pkg pkgRs of
          Just pkgR -> Just $ pkgR ^. remoteVersion
          Nothing -> Nothing
    , _packageDetailDTOState = computePackageState pkgRs pkg
    , _packageDetailDTORegistryLink = registryLink
    , _packageDetailDTOOrganization = selectOrganizationByOrgId pkg orgRs
    , _packageDetailDTOCreatedAt = pkg ^. createdAt
    }

toSuggestionDTO :: (PackageList, [String]) -> PackageSuggestionDTO
toSuggestionDTO (pkg, localVersions) =
  PackageSuggestionDTO
    { _packageSuggestionDTOPId = pkg ^. pId
    , _packageSuggestionDTOName = pkg ^. name
    , _packageSuggestionDTOVersion = pkg ^. version
    , _packageSuggestionDTODescription = pkg ^. description
    , _packageSuggestionDTOVersions = L.sortBy compareVersion localVersions
    }

fromDTO :: PackageDTO -> U.UUID -> PackageWithEvents
fromDTO dto appUuid =
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
    , _packageWithEventsEvents = dto ^. events
    , _packageWithEventsAppUuid = appUuid
    , _packageWithEventsCreatedAt = dto ^. createdAt
    }

buildPackageUrl :: String -> Package -> [RegistryPackage] -> Maybe String
buildPackageUrl clientRegistryUrl pkg pkgRs =
  case selectPackageByOrgIdAndKmId pkg pkgRs of
    Just pkgR ->
      Just $
      clientRegistryUrl ++
      "/knowledge-models/" ++ buildCoordinate (pkgR ^. organizationId) (pkgR ^. kmId) (pkgR ^. remoteVersion)
    Nothing -> Nothing
