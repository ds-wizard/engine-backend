module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates where

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates
import Wizard.Service.DocumentTemplate.DocumentTemplateMapper

wizardDocumentTemplateSimpleDTO :: DocumentTemplateSimpleDTO
wizardDocumentTemplateSimpleDTO =
  toSimpleDTO'
    True
    ( toList
        wizardDocumentTemplate
        (Just commonWizardRegistryTemplate)
        (Just globalRegistryOrganization)
        ReleasedDocumentTemplatePhase
    )

wizardDocumentTemplateDetailDTO :: DocumentTemplateDetailDTO
wizardDocumentTemplateDetailDTO =
  toDetailDTO
    wizardDocumentTemplate
    wizardDocumentTemplateFormats
    True
    [commonWizardRegistryTemplate]
    [globalRegistryOrganization]
    ["1.0.0"]
    (Just "https://registry-test.ds-wizard.org/document-templates/global:questionnaire-report:1.0.0")
    [globalKmPackage, netherlandsKmPackageV2]

wizardDocumentTemplateDeprecatedChangeDTO :: DocumentTemplateChangeDTO
wizardDocumentTemplateDeprecatedChangeDTO = toChangeDTO wizardDocumentTemplateDeprecated
