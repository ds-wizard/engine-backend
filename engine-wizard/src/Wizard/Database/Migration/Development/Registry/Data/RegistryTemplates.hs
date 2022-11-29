module Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates where

import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import Wizard.Model.Registry.RegistryTemplate

commonWizardRegistryTemplate :: RegistryTemplate
commonWizardRegistryTemplate =
  RegistryTemplate
    { organizationId = commonWizardTemplate.organizationId
    , templateId = commonWizardTemplate.templateId
    , remoteVersion = commonWizardTemplate.version
    , createdAt = commonWizardTemplate.createdAt
    }
