module Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateBundleDTO where
  declareNamedSchema = toSwagger (toBundle wizardDocumentTemplate [] [])
