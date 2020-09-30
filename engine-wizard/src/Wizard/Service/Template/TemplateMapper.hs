module Wizard.Service.Template.TemplateMapper where

import Control.Lens ((^.))
import qualified Data.List as L
import Data.Time

import LensesConfig
import qualified Registry.Api.Resource.Template.TemplateSimpleDTO as R_TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.Package.Package
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateGroup
import Shared.Util.Identifier
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import qualified Wizard.Service.Package.PackageMapper as PM_Mapper
import Wizard.Service.Template.TemplateUtil

toSimpleDTO :: Template -> TemplateSimpleDTO
toSimpleDTO = toSimpleDTO' [] [] []

toSimpleDTO' ::
     [R_TemplateSimpleDTO.TemplateSimpleDTO] -> [OrganizationSimpleDTO] -> [Package] -> Template -> TemplateSimpleDTO
toSimpleDTO' tmlRs orgRs pkgs tml =
  TemplateSimpleDTO
    { _templateSimpleDTOTId = tml ^. tId
    , _templateSimpleDTOName = tml ^. name
    , _templateSimpleDTOOrganizationId = tml ^. organizationId
    , _templateSimpleDTOTemplateId = tml ^. templateId
    , _templateSimpleDTOVersion = tml ^. version
    , _templateSimpleDTOMetamodelVersion = tml ^. metamodelVersion
    , _templateSimpleDTODescription = tml ^. description
    , _templateSimpleDTOReadme = tml ^. readme
    , _templateSimpleDTOLicense = tml ^. license
    , _templateSimpleDTOAllowedPackages = tml ^. allowedPackages
    , _templateSimpleDTORecommendedPackageId = tml ^. recommendedPackageId
    , _templateSimpleDTOFormats = tml ^. formats
    , _templateSimpleDTOUsablePackages = fmap PM_Mapper.toSimpleDTO . getUsablePackagesForTemplate tml $ pkgs
    , _templateSimpleDTOState = computeTemplateState tmlRs tml
    , _templateSimpleDTOOrganization = selectOrganizationByOrgId tml orgRs
    , _templateSimpleDTOCreatedAt = tml ^. createdAt
    }

toSimpleDTO'' ::
     [R_TemplateSimpleDTO.TemplateSimpleDTO]
  -> [OrganizationSimpleDTO]
  -> [Package]
  -> TemplateGroup
  -> TemplateSimpleDTO
toSimpleDTO'' tmlRs orgRs pkgs tmlGroup =
  let newest = L.maximumBy (\t1 t2 -> compare (t1 ^. version) (t2 ^. version)) (tmlGroup ^. versions)
   in toSimpleDTO' tmlRs orgRs pkgs newest

toDetailDTO ::
     Template
  -> [R_TemplateSimpleDTO.TemplateSimpleDTO]
  -> [OrganizationSimpleDTO]
  -> [String]
  -> String
  -> [Package]
  -> TemplateDetailDTO
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
          Just tmlR -> Just $ tmlR ^. version
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

fromCreateDTO :: TemplateChangeDTO -> UTCTime -> Template
fromCreateDTO dto createdAt =
  Template
    { _templateTId = buildIdentifierId (dto ^. organizationId) (dto ^. templateId) (dto ^. version)
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
    , _templateFiles = []
    , _templateAssets = []
    , _templateCreatedAt = createdAt
    }

fromChangeDTO :: TemplateChangeDTO -> Template -> Template
fromChangeDTO dto template =
  Template
    { _templateTId = buildIdentifierId (dto ^. organizationId) (dto ^. templateId) (dto ^. version)
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
    , _templateFiles = template ^. files
    , _templateAssets = template ^. assets
    , _templateCreatedAt = template ^. createdAt
    }

buildTemplateUrl :: String -> String -> String
buildTemplateUrl clientRegistryUrl tmlId = clientRegistryUrl ++ "/templates/" ++ tmlId
