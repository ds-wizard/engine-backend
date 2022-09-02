module Wizard.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Constant.Template
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.DefaultTemplate (html)
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import qualified Shared.Service.Package.PackageMapper as SPM
import Shared.Util.Uuid
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates
import Wizard.Service.Template.TemplateMapper

templateFileDefaultHtmlEditedChangeDto :: TemplateFileChangeDTO
templateFileDefaultHtmlEditedChangeDto =
  TemplateFileChangeDTO
    { _templateFileChangeDTOFileName = templateFileDefaultHtmlEdited ^. fileName
    , _templateFileChangeDTOContent = templateFileDefaultHtmlEdited ^. content
    }

templateFileNewFileChangeDto :: TemplateFileChangeDTO
templateFileNewFileChangeDto =
  TemplateFileChangeDTO
    { _templateFileChangeDTOFileName = templateFileNewFile ^. fileName
    , _templateFileChangeDTOContent = templateFileNewFile ^. content
    }

commonWizardTemplateSimpleDTO :: TemplateSimpleDTO
commonWizardTemplateSimpleDTO =
  toSimpleDTO'
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]
    (toTemplateList commonWizardTemplate (Just commonWizardRegistryTemplate) (Just globalRegistryOrganization))

commonWizardTemplateDetailDTO :: TemplateDetailDTO
commonWizardTemplateDetailDTO =
  toDetailDTO
    commonWizardTemplate
    [commonWizardRegistryTemplate]
    [globalRegistryOrganization]
    ["1.0.0"]
    (Just "https://registry-test.ds-wizard.org/templates/global:questionnaire-report:1.0.0")
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]

commonWizardTemplateEditedChangeDto :: TemplateChangeDTO
commonWizardTemplateEditedChangeDto = toChangeDTO commonWizardTemplateEdited

-- ---------------------------------------------------------------------------------------------------------------------
differentTemplate :: Template
differentTemplate =
  Template
    { _templateTId = "dsw:another-temlate:1.0.0"
    , _templateName = "Another Template"
    , _templateOrganizationId = "dsw"
    , _templateTemplateId = "another-template"
    , _templateVersion = "1.0.0"
    , _templateMetamodelVersion = templateMetamodelVersion
    , _templateDescription = "This is a another template"
    , _templateReadme = "# Another Template"
    , _templateLicense = "Apache-2.0"
    , _templateAllowedPackages = [packagePatternAll]
    , _templateRecommendedPackageId = Nothing
    , _templateFormats = []
    , _templateAppUuid = differentApp ^. uuid
    , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

differentFileHtml :: TemplateFile
differentFileHtml =
  TemplateFile
    { _templateFileTemplateId = differentTemplate ^. tId
    , _templateFileUuid = u' "2d9eb63d-05fb-4eb7-9dc3-378b55d062ce"
    , _templateFileFileName = "default.html.j2"
    , _templateFileContent = html
    , _templateFileAppUuid = differentApp ^. uuid
    }
