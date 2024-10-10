module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireFiles where

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Model.Questionnaire.QuestionnaireFileSimple
import Wizard.Model.Questionnaire.QuestionnaireSimple

questionnaireFileList :: QuestionnaireFileList
questionnaireFileList =
  QuestionnaireFileList
    { uuid = u' "e3726571-f81e-4e34-a17a-2f5714b7aade"
    , fileName = "my_file.txt"
    , contentType = "application/json"
    , fileSize = 123456
    , questionnaire =
        QuestionnaireSimple
          { uuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"
          , name = "My Private Questionnaire"
          }
    , createdBy = Just userAlbertSuggestion
    , createdAt = dt' 2018 01 21
    }

questionnaireFileSimple :: QuestionnaireFileSimple
questionnaireFileSimple =
  QuestionnaireFileSimple
    { uuid = questionnaireFileList.uuid
    , fileName = questionnaireFileList.fileName
    , contentType = questionnaireFileList.contentType
    , fileSize = questionnaireFileList.fileSize
    }
