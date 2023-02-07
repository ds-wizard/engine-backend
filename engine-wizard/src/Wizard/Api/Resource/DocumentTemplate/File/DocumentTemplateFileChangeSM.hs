module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles

instance ToSchema DocumentTemplateFileChangeDTO where
  declareNamedSchema = toSwagger fileDefaultHtmlEditedChangeDTO
