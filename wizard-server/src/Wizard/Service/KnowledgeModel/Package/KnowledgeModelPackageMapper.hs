module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import qualified Data.List as L

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageList
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil

toSimpleDTO :: KnowledgeModelPackage -> KnowledgeModelPackageSimpleDTO
toSimpleDTO = toSimpleDTO' [] []

toSimpleDTO' :: [RegistryPackage] -> [RegistryOrganization] -> KnowledgeModelPackage -> KnowledgeModelPackageSimpleDTO
toSimpleDTO' pkgRs orgRs pkg =
  KnowledgeModelPackageSimpleDTO
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

toSimpleDTO'' :: Bool -> KnowledgeModelPackageList -> KnowledgeModelPackageSimpleDTO
toSimpleDTO'' registryEnabled pkg =
  KnowledgeModelPackageSimpleDTO
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

toDetailDTO :: KnowledgeModelPackage -> Bool -> [RegistryPackage] -> [RegistryOrganization] -> [String] -> Maybe String -> KnowledgeModelPackageDetailDTO
toDetailDTO pkg registryEnabled pkgRs orgRs versionLs registryLink =
  KnowledgeModelPackageDetailDTO
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

toSuggestion :: KnowledgeModelPackage -> KnowledgeModelPackageSuggestion
toSuggestion pkg =
  KnowledgeModelPackageSuggestion
    { pId = pkg.pId
    , name = pkg.name
    , version = pkg.version
    , description = pkg.description
    }

toChangeDTO :: KnowledgeModelPackage -> KnowledgeModelPackageChangeDTO
toChangeDTO pkg =
  KnowledgeModelPackageChangeDTO
    { phase = pkg.phase
    }

buildPackageUrl :: String -> KnowledgeModelPackage -> [RegistryPackage] -> Maybe String
buildPackageUrl clientRegistryUrl pkg pkgRs =
  case selectPackageByOrgIdAndKmId pkg pkgRs of
    Just pkgR ->
      Just $
        clientRegistryUrl
          ++ "/knowledge-models/"
          ++ buildCoordinate pkgR.organizationId pkgR.kmId pkgR.remoteVersion
    Nothing -> Nothing
