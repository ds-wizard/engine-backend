module Wizard.Database.Migration.Development.Template.Data.Templates where

import Data.Maybe (fromJust)
import Data.Time

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
import Wizard.Model.App.App
import Wizard.Model.Template.TemplateState
import Wizard.Service.Template.TemplateMapper

templateFileDefaultHtmlEditedChangeDto :: TemplateFileChangeDTO
templateFileDefaultHtmlEditedChangeDto =
  TemplateFileChangeDTO
    { fileName = templateFileDefaultHtmlEdited.fileName
    , content = templateFileDefaultHtmlEdited.content
    }

templateFileNewFileChangeDto :: TemplateFileChangeDTO
templateFileNewFileChangeDto =
  TemplateFileChangeDTO
    { fileName = templateFileNewFile.fileName
    , content = templateFileNewFile.content
    }

commonWizardTemplateSimpleDTO :: TemplateSimpleDTO
commonWizardTemplateSimpleDTO =
  toSimpleDTO'
    True
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]
    ( toTemplateList
        commonWizardTemplate
        (Just commonWizardRegistryTemplate)
        (Just globalRegistryOrganization)
        UpToDateTemplateState
    )

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
    { tId = "dsw:another-temlate:1.0.0"
    , name = "Another Template"
    , organizationId = "dsw"
    , templateId = "another-template"
    , version = "1.0.0"
    , metamodelVersion = templateMetamodelVersion
    , description = "This is a another template"
    , readme = "# Another Template"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , recommendedPackageId = Nothing
    , formats = []
    , appUuid = differentApp.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

differentFileHtml :: TemplateFile
differentFileHtml =
  TemplateFile
    { templateId = differentTemplate.tId
    , uuid = u' "2d9eb63d-05fb-4eb7-9dc3-378b55d062ce"
    , fileName = "default.html.j2"
    , content = html
    , appUuid = differentApp.uuid
    }
