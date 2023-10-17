module Wizard.Database.Mapping.Questionnaire.QuestionnaireSimpleWithPerm where

import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Database.Mapping.Questionnaire.QuestionnairePerm ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireSimpleWithPerm

instance FromRow QuestionnaireSimpleWithPerm where
  fromRow = do
    uuid <- field
    visibility <- field
    sharing <- field
    tenantUuid <- field
    mUserPermissions <- fieldWith (optionalField fromField)
    let userPermissions =
          case mUserPermissions of
            Just userPermissions -> fmap (parseUserPermission uuid tenantUuid) . fromPGArray $ userPermissions
            Nothing -> []
    mGroupPermissions <- fieldWith (optionalField fromField)
    let groupPermissions =
          case mGroupPermissions of
            Just groupPermissions -> fmap (parseGroupPermission uuid tenantUuid) . fromPGArray $ groupPermissions
            Nothing -> []
    let permissions = userPermissions ++ groupPermissions
    return $ QuestionnaireSimpleWithPerm {..}
    where
      parseUserPermission :: U.UUID -> U.UUID -> String -> QuestionnairePerm
      parseUserPermission qtnUuid tenantUuid permission =
        let parts = splitOn "::" permission
         in QuestionnairePerm
              { questionnaireUuid = qtnUuid
              , memberType = UserQuestionnairePermType
              , memberUuid = u' $ head parts
              , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
              , tenantUuid = tenantUuid
              }
      parseGroupPermission :: U.UUID -> U.UUID -> String -> QuestionnairePerm
      parseGroupPermission qtnUuid tenantUuid permission =
        let parts = splitOn "::" permission
         in QuestionnairePerm
              { questionnaireUuid = qtnUuid
              , memberType = UserGroupQuestionnairePermType
              , memberUuid = u' $ head parts
              , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
              , tenantUuid = tenantUuid
              }
