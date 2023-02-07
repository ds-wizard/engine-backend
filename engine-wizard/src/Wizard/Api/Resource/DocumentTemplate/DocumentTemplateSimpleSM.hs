module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
