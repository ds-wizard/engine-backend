module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple

instance ToSchema DocumentTemplateSimple where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimple
