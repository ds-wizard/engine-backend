module Wizard.Service.Template.TemplateMapper where

import Control.Lens ((^.))
import Data.Time

import LensesConfig
import Shared.Model.Package.Package
import Shared.Model.Template.Template
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import qualified Wizard.Service.Package.PackageMapper as PM_Mapper
import Wizard.Util.IdentifierUtil

toDTO :: [Package] -> Template -> TemplateDTO
toDTO pkgs template =
  TemplateDTO
    { _templateDTOTId = template ^. tId
    , _templateDTOName = template ^. name
    , _templateDTOOrganizationId = template ^. organizationId
    , _templateDTOTemplateId = template ^. templateId
    , _templateDTOVersion = template ^. version
    , _templateDTOMetamodelVersion = template ^. metamodelVersion
    , _templateDTODescription = template ^. description
    , _templateDTOReadme = template ^. readme
    , _templateDTOLicense = template ^. license
    , _templateDTOAllowedPackages = fmap PM_Mapper.toSimpleDTO pkgs
    , _templateDTORecommendedPackageId = template ^. recommendedPackageId
    , _templateDTOFormats = template ^. formats
    , _templateDTOCreatedAt = template ^. createdAt
    }

toSimpleDTO :: Template -> TemplateSimpleDTO
toSimpleDTO template =
  TemplateSimpleDTO
    { _templateSimpleDTOTId = template ^. tId
    , _templateSimpleDTOName = template ^. name
    , _templateSimpleDTOOrganizationId = template ^. organizationId
    , _templateSimpleDTOTemplateId = template ^. templateId
    , _templateSimpleDTOVersion = template ^. version
    , _templateSimpleDTOMetamodelVersion = template ^. metamodelVersion
    , _templateSimpleDTODescription = template ^. description
    , _templateSimpleDTOReadme = template ^. readme
    , _templateSimpleDTOLicense = template ^. license
    , _templateSimpleDTOAllowedPackages = template ^. allowedPackages
    , _templateSimpleDTORecommendedPackageId = template ^. recommendedPackageId
    , _templateSimpleDTOFormats = template ^. formats
    , _templateSimpleDTOCreatedAt = template ^. createdAt
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
    { _templateTId = buildPackageId (dto ^. organizationId) (dto ^. templateId) (dto ^. version)
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
    { _templateTId = buildPackageId (dto ^. organizationId) (dto ^. templateId) (dto ^. version)
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
