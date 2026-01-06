module Wizard.Database.Migration.Development.Project.Data.ProjectFiles where

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.File.ProjectFileList
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.Project.ProjectSimple

projectFileList :: ProjectFileList
projectFileList =
  ProjectFileList
    { uuid = u' "e3726571-f81e-4e34-a17a-2f5714b7aade"
    , fileName = "my_file.txt"
    , contentType = "application/json"
    , fileSize = 123456
    , project =
        ProjectSimple
          { uuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"
          , name = "My Private Project"
          }
    , createdBy = Just userAlbertSuggestion
    , createdAt = dt' 2018 01 21
    }

projectFileSimple :: ProjectFileSimple
projectFileSimple =
  ProjectFileSimple
    { uuid = projectFileList.uuid
    , fileName = projectFileList.fileName
    , contentType = projectFileList.contentType
    , fileSize = projectFileList.fileSize
    }
