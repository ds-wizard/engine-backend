module Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Util.Uuid
import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.QuestionnaireAcl

instance ToRow QuestionnairePermRecord where
  toRow QuestionnairePermRecord {..} =
    case _questionnairePermRecordMember of
      m@GroupMember {..} ->
        [ toField _questionnairePermRecordUuid
        , toField _groupMemberGId
        , toField . PGArray $ _questionnairePermRecordPerms
        , toField _questionnairePermRecordQuestionnaireUuid
        ]
      m@UserMember {..} ->
        [ toField _questionnairePermRecordUuid
        , toField _userMemberUuid
        , toField . PGArray $ _questionnairePermRecordPerms
        , toField _questionnairePermRecordQuestionnaireUuid
        ]

instance FromRow QuestionnairePermRecord where
  fromRow = do
    _questionnairePermRecordUuid <- field
    aType <- field
    case aType of
      "GroupMember" -> do
        _groupMemberGId <- field
        let _questionnairePermRecordMember = GroupMember _groupMemberGId
        _questionnairePermRecordPerms <- fromPGArray <$> field
        _questionnairePermRecordQuestionnaireUuid <- field
        return $ QuestionnairePermRecord {..}
      "UserMember" -> do
        _userMemberUuid <- fmap u' field
        let _questionnairePermRecordMember = UserMember _userMemberUuid
        _questionnairePermRecordPerms <- fromPGArray <$> field
        _questionnairePermRecordQuestionnaireUuid <- field
        return $ QuestionnairePermRecord {..}
