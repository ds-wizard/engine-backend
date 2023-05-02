module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
