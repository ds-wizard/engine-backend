module Wizard.Service.DocumentTemplate.DocumentTemplateMapper where

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Package.Package
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Util.Coordinate
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryTemplate
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import qualified Wizard.Service.Package.PackageMapper as PM_Mapper

toList :: DocumentTemplate -> Maybe RegistryTemplate -> Maybe RegistryOrganization -> DocumentTemplateState -> DocumentTemplatePhase -> DocumentTemplateList
toList tml mTmlR mOrgR state phase =
  DocumentTemplateList
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = tml.phase
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
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
    , updatedAt = tml.updatedAt
    }

toSimpleDTO :: DocumentTemplate -> DocumentTemplateSimpleDTO
toSimpleDTO tml = toSimpleDTO' False [] $ toList tml Nothing Nothing UnknownDocumentTemplateState ReleasedDocumentTemplatePhase

toSimpleDTO' :: Bool -> [Package] -> DocumentTemplateList -> DocumentTemplateSimpleDTO
toSimpleDTO' registryEnabled pkgs tml =
  DocumentTemplateSimpleDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = tml.phase
    , remoteLatestVersion = tml.remoteVersion
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , formats = tml.formats
    , usablePackages = fmap PM_Mapper.toSimpleDTO . getUsablePackagesForDocumentTemplate tml $ pkgs
    , state = computeDocumentTemplateState' registryEnabled tml
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

toSuggestionDTO :: DocumentTemplateList -> DocumentTemplateSuggestionDTO
toSuggestionDTO tml =
  DocumentTemplateSuggestionDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , description = tml.description
    , formats = fmap toFormatDTO tml.formats
    }

toDetailDTO
  :: DocumentTemplate
  -> [RegistryTemplate]
  -> [RegistryOrganization]
  -> [String]
  -> Maybe String
  -> [Package]
  -> DocumentTemplateDetailDTO
toDetailDTO tml tmlRs orgRs versionLs registryLink pkgs =
  DocumentTemplateDetailDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = tml.phase
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , formats = tml.formats
    , usablePackages = fmap PM_Mapper.toSimpleDTO pkgs
    , versions = versionLs
    , remoteLatestVersion =
        case selectDocumentTemplateByOrgIdAndTmlId tml tmlRs of
          Just tmlR -> Just $ tmlR.remoteVersion
          Nothing -> Nothing
    , state = computeDocumentTemplateState tmlRs tml
    , registryLink = registryLink
    , organization = selectOrganizationByOrgId tml orgRs
    , createdAt = tml.createdAt
    }

toChangeDTO :: DocumentTemplate -> DocumentTemplateChangeDTO
toChangeDTO tml =
  DocumentTemplateChangeDTO
    { phase = tml.phase
    }

fromChangeDTO :: DocumentTemplateChangeDTO -> DocumentTemplate -> DocumentTemplate
fromChangeDTO dto tml =
  DocumentTemplate
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = dto.phase
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , formats = tml.formats
    , appUuid = tml.appUuid
    , createdAt = tml.createdAt
    , updatedAt = tml.updatedAt
    }

buildRegistryTemplateUrl :: String -> DocumentTemplate -> [RegistryTemplate] -> Maybe String
buildRegistryTemplateUrl clientRegistryUrl tml tmlRs =
  case selectDocumentTemplateByOrgIdAndTmlId tml tmlRs of
    Just tmlR ->
      Just $
        clientRegistryUrl
          ++ "/document-templates/"
          ++ buildCoordinate tmlR.organizationId tmlR.templateId tmlR.remoteVersion
    Nothing -> Nothing
