module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO

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
