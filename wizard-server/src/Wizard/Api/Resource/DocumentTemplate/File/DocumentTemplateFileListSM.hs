module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

instance ToSchema DocumentTemplateFileList where
  declareNamedSchema = toSwagger (toList fileDefaultHtml)
