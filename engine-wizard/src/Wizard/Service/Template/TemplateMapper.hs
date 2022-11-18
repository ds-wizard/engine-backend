module Wizard.Service.Template.TemplateMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Model.Package.Package
import Shared.Model.Template.Template
import Shared.Service.Template.TemplateMapper
import Shared.Util.Coordinate
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryTemplate
import Wizard.Model.Template.TemplateList
import Wizard.Model.Template.TemplateState
import qualified Wizard.Service.Package.PackageMapper as PM_Mapper
import Wizard.Service.Template.TemplateUtil

toTemplateList :: Template -> Maybe RegistryTemplate -> Maybe RegistryOrganization -> TemplateState -> TemplateList
toTemplateList tml mTmlR mOrgR state =
  TemplateList
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , recommendedPackageId = tml.recommendedPackageId
    , formats = tml.formats
    , state = state
    , remoteVersion = fmap (.remoteVersion) mTmlR
    , remoteOrganizationName = fmap (.name) mOrgR
    , remoteOrganizationLogo =
        case mOrgR of
          Just orgR -> orgR.logo
          Nothing -> Nothing
    , appUuid = tml.appUuid
    , createdAt = tml.createdAt
    }

toSimpleDTO :: Template -> TemplateSimpleDTO
toSimpleDTO tml = toSimpleDTO' False [] $ toTemplateList tml Nothing Nothing UnknownTemplateState

toSimpleDTO' :: Bool -> [Package] -> TemplateList -> TemplateSimpleDTO
toSimpleDTO' registryEnabled pkgs tml =
  TemplateSimpleDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , remoteLatestVersion = tml.remoteVersion
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , recommendedPackageId = tml.recommendedPackageId
    , formats = tml.formats
    , usablePackages = fmap PM_Mapper.toSimpleDTO . getUsablePackagesForTemplate tml $ pkgs
    , state = computeTemplateState' registryEnabled tml
    , organization =
        case tml.remoteOrganizationName of
          Just orgName ->
            Just $
              OrganizationSimpleDTO
                { organizationId = tml.organizationId
                , name = orgName
                , logo = tml.remoteOrganizationLogo
                }
          Nothing -> Nothing
    , createdAt = tml.createdAt
    }

toSuggestionDTO :: TemplateList -> TemplateSuggestionDTO
toSuggestionDTO tml =
  TemplateSuggestionDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , description = tml.description
    , formats = fmap toFormatDTO tml.formats
    }

toDetailDTO
  :: Template
  -> [RegistryTemplate]
  -> [RegistryOrganization]
  -> [String]
  -> Maybe String
  -> [Package]
  -> TemplateDetailDTO
toDetailDTO tml tmlRs orgRs versionLs registryLink pkgs =
  TemplateDetailDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , recommendedPackageId = tml.recommendedPackageId
    , formats = tml.formats
    , usablePackages = fmap PM_Mapper.toSimpleDTO pkgs
    , versions = versionLs
    , remoteLatestVersion =
        case selectTemplateByOrgIdAndTmlId tml tmlRs of
          Just tmlR -> Just $ tmlR.remoteVersion
          Nothing -> Nothing
    , state = computeTemplateState tmlRs tml
    , registryLink = registryLink
    , organization = selectOrganizationByOrgId tml orgRs
    , createdAt = tml.createdAt
    }

toChangeDTO :: Template -> TemplateChangeDTO
toChangeDTO template =
  TemplateChangeDTO
    { name = template.name
    , organizationId = template.organizationId
    , templateId = template.templateId
    , version = template.version
    , metamodelVersion = template.metamodelVersion
    , description = template.description
    , readme = template.readme
    , license = template.license
    , allowedPackages = template.allowedPackages
    , recommendedPackageId = template.recommendedPackageId
    , formats = template.formats
    }

fromCreateDTO :: TemplateChangeDTO -> U.UUID -> UTCTime -> Template
fromCreateDTO dto appUuid createdAt =
  Template
    { tId = buildCoordinate dto.organizationId dto.templateId dto.version
    , name = dto.name
    , organizationId = dto.organizationId
    , templateId = dto.templateId
    , version = dto.version
    , metamodelVersion = dto.metamodelVersion
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , allowedPackages = dto.allowedPackages
    , recommendedPackageId = dto.recommendedPackageId
    , formats = dto.formats
    , appUuid = appUuid
    , createdAt = createdAt
    }

fromChangeDTO :: TemplateChangeDTO -> Template -> Template
fromChangeDTO dto template =
  Template
    { tId = buildCoordinate dto.organizationId dto.templateId dto.version
    , name = dto.name
    , organizationId = dto.organizationId
    , templateId = dto.templateId
    , version = dto.version
    , metamodelVersion = dto.metamodelVersion
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , allowedPackages = dto.allowedPackages
    , recommendedPackageId = dto.recommendedPackageId
    , formats = dto.formats
    , appUuid = template.appUuid
    , createdAt = template.createdAt
    }

buildTemplateUrl :: String -> Template -> [RegistryTemplate] -> Maybe String
buildTemplateUrl clientRegistryUrl tml tmlRs =
  case selectTemplateByOrgIdAndTmlId tml tmlRs of
    Just tmlR ->
      Just $
        clientRegistryUrl
          ++ "/templates/"
          ++ buildCoordinate tmlR.organizationId tmlR.templateId tmlR.remoteVersion
    Nothing -> Nothing
