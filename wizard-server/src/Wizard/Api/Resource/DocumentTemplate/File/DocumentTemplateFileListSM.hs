module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper

instance ToSchema DocumentTemplateFileList where
  declareNamedSchema = toSwagger (toList fileDefaultHtml)
