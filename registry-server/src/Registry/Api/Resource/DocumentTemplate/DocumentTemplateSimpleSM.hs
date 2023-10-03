module Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Registry.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Common.Util.Swagger

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
