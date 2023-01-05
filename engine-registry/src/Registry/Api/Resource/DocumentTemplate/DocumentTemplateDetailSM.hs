module Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM where

import Data.Swagger

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Registry.Api.Resource.Package.PackageSimpleSM ()
import Registry.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDetailDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDetailDTO
