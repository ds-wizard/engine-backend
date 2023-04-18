module Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates where

import Wizard.Model.Registry.RegistryTemplate
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

commonWizardRegistryTemplate :: RegistryTemplate
commonWizardRegistryTemplate =
  RegistryTemplate
    { organizationId = wizardDocumentTemplate.organizationId
    , templateId = wizardDocumentTemplate.templateId
    , remoteVersion = wizardDocumentTemplate.version
    , createdAt = wizardDocumentTemplate.createdAt
    }
