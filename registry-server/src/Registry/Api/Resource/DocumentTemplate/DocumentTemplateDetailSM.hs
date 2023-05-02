module Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM where

import Data.Swagger

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Registry.Api.Resource.Package.PackageSimpleSM ()
import Registry.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Common.Util.Swagger
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()

instance ToSchema DocumentTemplateDetailDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDetailDTO
