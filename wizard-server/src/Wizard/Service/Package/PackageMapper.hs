module Wizard.Service.Package.PackageMapper where

import qualified Data.List as L

import Wizard.Api.Resource.Package.PackageChangeDTO
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Package.PackageList
import Wizard.Model.Package.PackageSuggestion
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Service.Package.PackageUtil
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Model.Package.Package

toSimpleDTO :: Package -> PackageSimpleDTO
toSimpleDTO = toSimpleDTO' [] []

toSimpleDTO' :: [RegistryPackage] -> [RegistryOrganization] -> Package -> PackageSimpleDTO
toSimpleDTO' pkgRs orgRs pkg =
  PackageSimpleDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , remoteLatestVersion =
        case selectPackageByOrgIdAndKmId pkg pkgRs of
          Just pkgR -> Just $ pkgR.remoteVersion
          Nothing -> Nothing
    , description = pkg.description
    , nonEditable = pkg.nonEditable
    , organization = selectOrganizationByOrgId pkg orgRs
    , createdAt = pkg.createdAt
    }

toSimpleDTO'' :: Bool -> PackageList -> PackageSimpleDTO
toSimpleDTO'' registryEnabled pkg =
  PackageSimpleDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , remoteLatestVersion =
        if registryEnabled
          then pkg.remoteVersion
          else Nothing
    , description = pkg.description
    , nonEditable = pkg.nonEditable
    , organization =
        case (registryEnabled, pkg.remoteOrganizationName) of
          (True, Just orgName) ->
            Just $
              RegistryOrganization
                { organizationId = pkg.organizationId
                , name = orgName
                , logo = pkg.remoteOrganizationLogo
                , createdAt = pkg.createdAt
                }
          _ -> Nothing
    , createdAt = pkg.createdAt
    }

toDetailDTO :: Package -> Bool -> [RegistryPackage] -> [RegistryOrganization] -> [String] -> Maybe String -> PackageDetailDTO
toDetailDTO pkg registryEnabled pkgRs orgRs versionLs registryLink =
  PackageDetailDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , metamodelVersion = pkg.metamodelVersion
    , previousPackageId = pkg.previousPackageId
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , nonEditable = pkg.nonEditable
    , versions = L.sort versionLs
    , remoteLatestVersion =
        case (registryEnabled, selectPackageByOrgIdAndKmId pkg pkgRs) of
          (True, Just pkgR) -> Just $ pkgR.remoteVersion
          _ -> Nothing
    , registryLink =
        if registryEnabled
          then registryLink
          else Nothing
    , organization =
        if registryEnabled
          then selectOrganizationByOrgId pkg orgRs
          else Nothing
    , createdAt = pkg.createdAt
    }

toSuggestion :: Package -> PackageSuggestion
toSuggestion pkg =
  PackageSuggestion
    { pId = pkg.pId
    , name = pkg.name
    , version = pkg.version
    , description = pkg.description
    }

toChangeDTO :: Package -> PackageChangeDTO
toChangeDTO pkg =
  PackageChangeDTO
    { phase = pkg.phase
    }

buildPackageUrl :: String -> Package -> [RegistryPackage] -> Maybe String
buildPackageUrl clientRegistryUrl pkg pkgRs =
  case selectPackageByOrgIdAndKmId pkg pkgRs of
    Just pkgR ->
      Just $
        clientRegistryUrl
          ++ "/knowledge-models/"
          ++ buildCoordinate pkgR.organizationId pkgR.kmId pkgR.remoteVersion
    Nothing -> Nothing
