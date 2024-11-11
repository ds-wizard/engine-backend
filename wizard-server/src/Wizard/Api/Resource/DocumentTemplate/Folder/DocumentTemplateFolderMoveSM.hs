module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders

instance ToSchema DocumentTemplateFolderMoveDTO where
  declareNamedSchema = toSwagger folderMoveDto
