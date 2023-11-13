module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates where

import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Service.DocumentTemplate.DocumentTemplateMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SPM

wizardDocumentTemplateSimpleDTO :: DocumentTemplateSimpleDTO
wizardDocumentTemplateSimpleDTO =
  toSimpleDTO'
    True
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
