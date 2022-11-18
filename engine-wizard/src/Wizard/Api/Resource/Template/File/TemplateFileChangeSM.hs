module Wizard.Api.Resource.Template.File.TemplateFileChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.File.TemplateFileChangeJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates

instance ToSchema TemplateFileChangeDTO where
  declareNamedSchema = toSwagger templateFileDefaultHtmlEditedChangeDto
