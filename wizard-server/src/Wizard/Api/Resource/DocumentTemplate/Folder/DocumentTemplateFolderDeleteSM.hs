module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders

instance ToSchema DocumentTemplateFolderDeleteDTO where
  declareNamedSchema = toSwagger folderDeleteDto
