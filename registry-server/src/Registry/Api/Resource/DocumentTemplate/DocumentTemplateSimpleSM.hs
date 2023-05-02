module Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Registry.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Common.Util.Swagger
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
