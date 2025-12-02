module Wizard.Service.DocumentTemplate.DocumentTemplateMapper where

import Data.Maybe (fromMaybe)

import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Coordinate.Util.Coordinate
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryTemplate
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PM_Mapper

toList :: DocumentTemplate -> Maybe RegistryTemplate -> Maybe RegistryOrganization -> DocumentTemplatePhase -> DocumentTemplateList
toList tml mTmlR mOrgR phase =
  DocumentTemplateList
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = tml.phase
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , allowedPackages = tml.allowedPackages
    , nonEditable = tml.nonEditable
    , remoteVersion = fmap (.remoteVersion) mTmlR
    , remoteOrganizationName = fmap (.name) mOrgR
    , remoteOrganizationLogo =
        case mOrgR of
          Just orgR -> orgR.logo
          Nothing -> Nothing
    , createdAt = tml.createdAt
    }

toSimpleDTO :: DocumentTemplate -> DocumentTemplateSimpleDTO
toSimpleDTO tml = toSimpleDTO' False $ toList tml Nothing Nothing ReleasedDocumentTemplatePhase

toSimpleDTO' :: Bool -> DocumentTemplateList -> DocumentTemplateSimpleDTO
toSimpleDTO' registryEnabled tml =
  DocumentTemplateSimpleDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = tml.phase
    , remoteLatestVersion =
        if registryEnabled
          then tml.remoteVersion
          else Nothing
    , description = tml.description
    , nonEditable = tml.nonEditable
    , state = computeDocumentTemplateState' tml
    , organization =
        case (registryEnabled, tml.remoteOrganizationName) of
          (True, Just orgName) ->
            Just $
              OrganizationSimple
                { organizationId = tml.organizationId
                , name = orgName
                , logo = tml.remoteOrganizationLogo
                }
          _ -> Nothing
    , createdAt = tml.createdAt
    }

toSuggestionDTOPage :: [DocumentTemplateSuggestion] -> Pageable -> Page DocumentTemplateSuggestionDTO
toSuggestionDTOPage suggestions pageable =
  let size = fromMaybe 20 pageable.size
      totalElements = length suggestions
      metadata =
        PageMetadata
          { size = size
          , totalElements = totalElements
          , totalPages = computeTotalPage totalElements size
          , number = fromMaybe 0 pageable.page
          }
      entities = fmap toSuggestionDTO . take size $ suggestions
   in Page "documentTemplates" metadata entities

toSuggestionDTO :: DocumentTemplateSuggestion -> DocumentTemplateSuggestionDTO
toSuggestionDTO DocumentTemplateSuggestion {..} = DocumentTemplateSuggestionDTO {..}

toDetailDTO
  :: DocumentTemplate
  -> [DocumentTemplateFormat]
  -> Bool
  -> [RegistryTemplate]
  -> [RegistryOrganization]
  -> [String]
  -> Maybe String
  -> [KnowledgeModelPackage]
  -> DocumentTemplateDetailDTO
toDetailDTO tml formats registryEnabled tmlRs orgRs versionLs registryLink pkgs =
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
    , formats = formats
    , nonEditable = tml.nonEditable
    , usableKnowledgeModels = fmap PM_Mapper.toSimpleDTO pkgs
    , versions = versionLs
    , remoteLatestVersion =
        case (registryEnabled, selectDocumentTemplateByOrgIdAndTmlId tml tmlRs) of
          (True, Just tmlR) -> Just $ tmlR.remoteVersion
          _ -> Nothing
    , state = computeDocumentTemplateState tmlRs tml
    , registryLink =
        if registryEnabled
          then registryLink
          else Nothing
    , organization =
        if registryEnabled
          then selectOrganizationByOrgId tml orgRs
          else Nothing
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
    , nonEditable = tml.nonEditable
    , tenantUuid = tml.tenantUuid
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
