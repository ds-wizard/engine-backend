module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles where

import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

fileDefaultHtmlEditedChangeDTO :: DocumentTemplateFileChangeDTO
fileDefaultHtmlEditedChangeDTO =
  DocumentTemplateFileChangeDTO
    { fileName = fileDefaultHtmlEdited.fileName
    , content = fileDefaultHtmlEdited.content
    }

fileNewFileChangeDTO :: DocumentTemplateFileChangeDTO
fileNewFileChangeDTO =
  DocumentTemplateFileChangeDTO
    { fileName = fileNewFile.fileName
    , content = fileNewFile.content
    }
