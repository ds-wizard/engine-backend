module Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Registry.RegistryTemplate

commonWizardRegistryTemplate :: RegistryTemplate
commonWizardRegistryTemplate =
  RegistryTemplate
    { _registryTemplateOrganizationId = commonWizardTemplate ^. organizationId
    , _registryTemplateTemplateId = commonWizardTemplate ^. templateId
    , _registryTemplateRemoteVersion = commonWizardTemplate ^. version
    , _registryTemplateCreatedAt = commonWizardTemplate ^. createdAt
    }
