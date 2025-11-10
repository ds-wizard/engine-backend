module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
