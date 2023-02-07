module Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Model.Registry.RegistryTemplate

commonWizardRegistryTemplate :: RegistryTemplate
commonWizardRegistryTemplate =
  RegistryTemplate
    { organizationId = wizardDocumentTemplate.organizationId
    , templateId = wizardDocumentTemplate.templateId
    , remoteVersion = wizardDocumentTemplate.version
    , createdAt = wizardDocumentTemplate.createdAt
    }
