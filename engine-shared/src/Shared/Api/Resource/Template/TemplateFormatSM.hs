module Shared.Api.Resource.Template.TemplateFormatSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateFormatDTO
import Shared.Api.Resource.Template.TemplateFormatJM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Service.Template.TemplateMapper
import Shared.Util.Swagger

instance ToSchema TemplateFormatDTO where
  declareNamedSchema = toSwagger (toFormatDTO templateFormatJson)
