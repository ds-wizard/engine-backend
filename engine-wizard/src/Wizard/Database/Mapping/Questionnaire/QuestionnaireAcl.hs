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
  toRow r@QuestionnairePermRecord {..} =
    case member of
      m@GroupMember {..} ->
        [ toField r.uuid
        , toField m.gId
        , toField . PGArray $ r.perms
        , toField r.questionnaireUuid
        ]
      m@UserMember {..} ->
        [ toField r.uuid
        , toField m.uuid
        , toField . PGArray $ r.perms
        , toField r.questionnaireUuid
        ]

instance FromRow QuestionnairePermRecord where
  fromRow = do
    rUuid <- field
    aType <- field
    case aType of
      "GroupMember" -> do
        gId <- field
        let member = GroupMember gId
        perms <- fromPGArray <$> field
        questionnaireUuid <- field
        return
          QuestionnairePermRecord
            { uuid = rUuid
            , questionnaireUuid = questionnaireUuid
            , perms = perms
            , member = member
            }
      "UserMember" -> do
        uuid <- fmap u' field
        let member = UserMember uuid
        perms <- fromPGArray <$> field
        questionnaireUuid <- field
        return
          QuestionnairePermRecord
            { uuid = rUuid
            , questionnaireUuid = questionnaireUuid
            , perms = perms
            , member = member
            }
