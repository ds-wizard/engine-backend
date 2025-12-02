module Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper

instance ToSchema DocumentTemplateBundleDTO where
  declareNamedSchema = toSwagger (toBundle wizardDocumentTemplate [] [] [])
