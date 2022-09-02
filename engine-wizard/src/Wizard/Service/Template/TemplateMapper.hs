module Wizard.Service.Template.TemplateMapper where

import Control.Lens ((^.), (^?), _Just)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
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
import qualified Wizard.Service.Package.PackageMapper as PM_Mapper
import Wizard.Service.Template.TemplateUtil

toTemplateList :: Template -> Maybe RegistryTemplate -> Maybe RegistryOrganization -> TemplateList
toTemplateList tml mTmlR mOrgR =
  TemplateList
    { _templateListTId = tml ^. tId
    , _templateListName = tml ^. name
    , _templateListOrganizationId = tml ^. organizationId
    , _templateListTemplateId = tml ^. templateId
    , _templateListVersion = tml ^. version
    , _templateListMetamodelVersion = tml ^. metamodelVersion
    , _templateListDescription = tml ^. description
    , _templateListReadme = tml ^. readme
    , _templateListLicense = tml ^. license
    , _templateListAllowedPackages = tml ^. allowedPackages
    , _templateListRecommendedPackageId = tml ^. recommendedPackageId
    , _templateListFormats = tml ^. formats
    , _templateListRemoteVersion = mTmlR ^? _Just . remoteVersion
    , _templateListRemoteOrganizationName = mOrgR ^? _Just . name
    , _templateListRemoteOrganizationLogo =
        case mOrgR of
          Just orgR -> orgR ^. logo
          Nothing -> Nothing
    , _templateListAppUuid = tml ^. appUuid
    , _templateListCreatedAt = tml ^. createdAt
    }

toSimpleDTO :: Template -> TemplateSimpleDTO
toSimpleDTO tml = toSimpleDTO' [] $ toTemplateList tml Nothing Nothing

toSimpleDTO' :: [Package] -> TemplateList -> TemplateSimpleDTO
toSimpleDTO' pkgs tml =
  TemplateSimpleDTO
    { _templateSimpleDTOTId = tml ^. tId
    , _templateSimpleDTOName = tml ^. name
    , _templateSimpleDTOOrganizationId = tml ^. organizationId
    , _templateSimpleDTOTemplateId = tml ^. templateId
    , _templateSimpleDTOVersion = tml ^. version
    , _templateSimpleDTORemoteLatestVersion = tml ^. remoteVersion
    , _templateSimpleDTOMetamodelVersion = tml ^. metamodelVersion
    , _templateSimpleDTODescription = tml ^. description
    , _templateSimpleDTOReadme = tml ^. readme
    , _templateSimpleDTOLicense = tml ^. license
    , _templateSimpleDTOAllowedPackages = tml ^. allowedPackages
    , _templateSimpleDTORecommendedPackageId = tml ^. recommendedPackageId
    , _templateSimpleDTOFormats = tml ^. formats
    , _templateSimpleDTOUsablePackages = fmap PM_Mapper.toSimpleDTO . getUsablePackagesForTemplate tml $ pkgs
    , _templateSimpleDTOState = computeTemplateState' tml
    , _templateSimpleDTOOrganization =
        case tml ^. remoteOrganizationName of
          Just orgName ->
            Just $
            OrganizationSimpleDTO
              { _organizationSimpleDTOOrganizationId = tml ^. organizationId
              , _organizationSimpleDTOName = orgName
              , _organizationSimpleDTOLogo = tml ^. remoteOrganizationLogo
              }
          Nothing -> Nothing
    , _templateSimpleDTOCreatedAt = tml ^. createdAt
    }

toSuggestionDTO :: TemplateList -> TemplateSuggestionDTO
toSuggestionDTO tml =
  TemplateSuggestionDTO
    { _templateSuggestionDTOTId = tml ^. tId
    , _templateSuggestionDTOName = tml ^. name
    , _templateSuggestionDTOVersion = tml ^. version
    , _templateSuggestionDTODescription = tml ^. description
    , _templateSuggestionDTOFormats = fmap toFormatDTO (tml ^. formats)
    }

toDetailDTO ::
     Template -> [RegistryTemplate] -> [RegistryOrganization] -> [String] -> String -> [Package] -> TemplateDetailDTO
toDetailDTO tml tmlRs orgRs versionLs registryLink pkgs =
  TemplateDetailDTO
    { _templateDetailDTOTId = tml ^. tId
    , _templateDetailDTOName = tml ^. name
    , _templateDetailDTOOrganizationId = tml ^. organizationId
    , _templateDetailDTOTemplateId = tml ^. templateId
    , _templateDetailDTOVersion = tml ^. version
    , _templateDetailDTOMetamodelVersion = tml ^. metamodelVersion
    , _templateDetailDTODescription = tml ^. description
    , _templateDetailDTOReadme = tml ^. readme
    , _templateDetailDTOLicense = tml ^. license
    , _templateDetailDTOAllowedPackages = tml ^. allowedPackages
    , _templateDetailDTORecommendedPackageId = tml ^. recommendedPackageId
    , _templateDetailDTOFormats = tml ^. formats
    , _templateDetailDTOUsablePackages = fmap PM_Mapper.toSimpleDTO pkgs
    , _templateDetailDTOVersions = versionLs
    , _templateDetailDTORemoteLatestVersion =
        case selectTemplateByOrgIdAndTmlId tml tmlRs of
          Just tmlR -> Just $ tmlR ^. remoteVersion
          Nothing -> Nothing
    , _templateDetailDTOState = computeTemplateState tmlRs tml
    , _templateDetailDTORegistryLink =
        case selectTemplateByOrgIdAndTmlId tml tmlRs of
          Just tmlR -> Just registryLink
          Nothing -> Nothing
    , _templateDetailDTOOrganization = selectOrganizationByOrgId tml orgRs
    , _templateDetailDTOCreatedAt = tml ^. createdAt
    }

toChangeDTO :: Template -> TemplateChangeDTO
toChangeDTO template =
  TemplateChangeDTO
    { _templateChangeDTOName = template ^. name
    , _templateChangeDTOOrganizationId = template ^. organizationId
    , _templateChangeDTOTemplateId = template ^. templateId
    , _templateChangeDTOVersion = template ^. version
    , _templateChangeDTOMetamodelVersion = template ^. metamodelVersion
    , _templateChangeDTODescription = template ^. description
    , _templateChangeDTOReadme = template ^. readme
    , _templateChangeDTOLicense = template ^. license
    , _templateChangeDTOAllowedPackages = template ^. allowedPackages
    , _templateChangeDTORecommendedPackageId = template ^. recommendedPackageId
    , _templateChangeDTOFormats = template ^. formats
    }

fromCreateDTO :: TemplateChangeDTO -> U.UUID -> UTCTime -> Template
fromCreateDTO dto appUuid createdAt =
  Template
    { _templateTId = buildCoordinate (dto ^. organizationId) (dto ^. templateId) (dto ^. version)
    , _templateName = dto ^. name
    , _templateOrganizationId = dto ^. organizationId
    , _templateTemplateId = dto ^. templateId
    , _templateVersion = dto ^. version
    , _templateMetamodelVersion = dto ^. metamodelVersion
    , _templateDescription = dto ^. description
    , _templateReadme = dto ^. readme
    , _templateLicense = dto ^. license
    , _templateAllowedPackages = dto ^. allowedPackages
    , _templateRecommendedPackageId = dto ^. recommendedPackageId
    , _templateFormats = dto ^. formats
    , _templateAppUuid = appUuid
    , _templateCreatedAt = createdAt
    }

fromChangeDTO :: TemplateChangeDTO -> Template -> Template
fromChangeDTO dto template =
  Template
    { _templateTId = buildCoordinate (dto ^. organizationId) (dto ^. templateId) (dto ^. version)
    , _templateName = dto ^. name
    , _templateOrganizationId = dto ^. organizationId
    , _templateTemplateId = dto ^. templateId
    , _templateVersion = dto ^. version
    , _templateMetamodelVersion = dto ^. metamodelVersion
    , _templateDescription = dto ^. description
    , _templateReadme = dto ^. readme
    , _templateLicense = dto ^. license
    , _templateAllowedPackages = dto ^. allowedPackages
    , _templateRecommendedPackageId = dto ^. recommendedPackageId
    , _templateFormats = dto ^. formats
    , _templateAppUuid = template ^. appUuid
    , _templateCreatedAt = template ^. createdAt
    }

buildTemplateUrl :: String -> String -> String
buildTemplateUrl clientRegistryUrl tmlId = clientRegistryUrl ++ "/templates/" ++ tmlId
