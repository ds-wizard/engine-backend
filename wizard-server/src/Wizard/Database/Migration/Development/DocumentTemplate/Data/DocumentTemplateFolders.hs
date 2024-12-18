module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders where

import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO

folderMoveDto :: DocumentTemplateFolderMoveDTO
folderMoveDto =
  DocumentTemplateFolderMoveDTO
    { current = "src"
    , new = "src-new"
    }

folderDeleteDto :: DocumentTemplateFolderDeleteDTO
folderDeleteDto =
  DocumentTemplateFolderDeleteDTO
    { path = "src"
    }
