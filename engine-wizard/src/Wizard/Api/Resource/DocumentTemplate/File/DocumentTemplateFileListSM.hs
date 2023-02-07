module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListSM where

import Data.Swagger

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper

instance ToSchema DocumentTemplateFileList where
  declareNamedSchema = toSwagger (toList fileDefaultHtml)
