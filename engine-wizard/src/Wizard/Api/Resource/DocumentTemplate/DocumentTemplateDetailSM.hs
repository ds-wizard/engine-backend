module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

instance ToSchema DocumentTemplateDetailDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDetailDTO
