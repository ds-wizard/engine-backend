module Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateFormatDTO where
  declareNamedSchema = toSwagger (toFormatDTO formatJson)
