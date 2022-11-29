module Shared.Api.Resource.Template.TemplateSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateFormatSM ()
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Api.Resource.Template.TemplateSuggestionJM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Service.Template.TemplateMapper
import Shared.Util.Swagger

instance ToSchema TemplateSuggestionDTO where
  declareNamedSchema = toSwagger (toSuggestionDTO commonWizardTemplate)
