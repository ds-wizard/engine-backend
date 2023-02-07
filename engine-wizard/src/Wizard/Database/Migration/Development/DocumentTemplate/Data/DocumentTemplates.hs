module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.DocumentTemplate.DocumentTemplate
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Service.DocumentTemplate.DocumentTemplateMapper

wizardDocumentTemplateSimpleDTO :: DocumentTemplateSimpleDTO
wizardDocumentTemplateSimpleDTO =
  toSimpleDTO'
    True
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]
    ( toList
        wizardDocumentTemplate
        (Just commonWizardRegistryTemplate)
        (Just globalRegistryOrganization)
        UpToDateDocumentTemplateState
        ReleasedDocumentTemplatePhase
    )

wizardDocumentTemplateDetailDTO :: DocumentTemplateDetailDTO
wizardDocumentTemplateDetailDTO =
  toDetailDTO
    wizardDocumentTemplate
    [commonWizardRegistryTemplate]
    [globalRegistryOrganization]
    ["1.0.0"]
    (Just "https://registry-test.ds-wizard.org/document-templates/global:questionnaire-report:1.0.0")
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]

wizardDocumentTemplateDeprecatedChangeDTO :: DocumentTemplateChangeDTO
wizardDocumentTemplateDeprecatedChangeDTO = toChangeDTO wizardDocumentTemplateDeprecated
