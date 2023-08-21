module Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Registry.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Common.Util.Swagger
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
