module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateSuggestionDTO where
  declareNamedSchema = toSwagger (toSuggestionDTO wizardDocumentTemplate)
