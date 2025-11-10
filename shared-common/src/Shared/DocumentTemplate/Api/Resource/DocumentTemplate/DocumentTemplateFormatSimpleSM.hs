module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

instance ToSchema DocumentTemplateFormatSimple where
  declareNamedSchema = toSwagger formatJsonSimple
