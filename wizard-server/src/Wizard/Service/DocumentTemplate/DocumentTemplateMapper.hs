module Wizard.Service.DocumentTemplate.DocumentTemplateMapper where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Service.Version.VersionMapper
import Shared.Coordinate.Util.Coordinate
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion
import Wizard.Model.DocumentTemplate.DocumentTemplateWithCoordinate
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryTemplate
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PM_Mapper

toList :: DocumentTemplate -> Maybe RegistryTemplate -> Maybe RegistryOrganization -> DocumentTemplatePhase -> DocumentTemplateList
toList dt mDtR mOrgR phase =
  DocumentTemplateList
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , phase = dt.phase
    , metamodelVersion = dt.metamodelVersion
    , description = dt.description
    , allowedPackages = dt.allowedPackages
    , nonEditable = dt.nonEditable
    , remoteVersion = fmap (.remoteVersion) mDtR
    , remoteOrganizationName = fmap (.name) mOrgR
    , remoteOrganizationLogo =
        case mOrgR of
          Just orgR -> orgR.logo
          Nothing -> Nothing
    , createdAt = dt.createdAt
    }

toSimpleDTO :: DocumentTemplate -> DocumentTemplateSimpleDTO
toSimpleDTO dt = toSimpleDTO' False $ toList dt Nothing Nothing ReleasedDocumentTemplatePhase

toSimpleDTO' :: Bool -> DocumentTemplateList -> DocumentTemplateSimpleDTO
toSimpleDTO' registryEnabled dt =
  DocumentTemplateSimpleDTO
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , phase = dt.phase
    , remoteLatestVersion =
        if registryEnabled
          then dt.remoteVersion
          else Nothing
    , description = dt.description
    , nonEditable = dt.nonEditable
    , state = computeDocumentTemplateState' dt
    , organization =
        case (registryEnabled, dt.remoteOrganizationName) of
          (True, Just orgName) ->
            Just $
              OrganizationSimple
                { organizationId = dt.organizationId
                , name = orgName
                , logo = dt.remoteOrganizationLogo
                }
          _ -> Nothing
    , createdAt = dt.createdAt
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

toWithCoordinate :: DocumentTemplate -> DocumentTemplateWithCoordinate
toWithCoordinate DocumentTemplate {..} = DocumentTemplateWithCoordinate {..}

toDetailDTO
  :: DocumentTemplate
  -> [DocumentTemplateFormat]
  -> Bool
  -> [RegistryTemplate]
  -> [RegistryOrganization]
  -> [(U.UUID, String)]
  -> Maybe String
  -> [KnowledgeModelPackage]
  -> DocumentTemplateDetailDTO
toDetailDTO tml formats registryEnabled tmlRs orgRs versionLs registryLink pkgs =
  DocumentTemplateDetailDTO
    { uuid = tml.uuid
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
    , versions = map toVersionDTO . L.sortBy (\(_, v1) (_, v2) -> compare v2 v1) $ versionLs
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
fromChangeDTO dto dt =
  DocumentTemplate
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , phase = dto.phase
    , metamodelVersion = dt.metamodelVersion
    , description = dt.description
    , readme = dt.readme
    , license = dt.license
    , allowedPackages = dt.allowedPackages
    , nonEditable = dt.nonEditable
    , tenantUuid = dt.tenantUuid
    , createdAt = dt.createdAt
    , updatedAt = dt.updatedAt
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
