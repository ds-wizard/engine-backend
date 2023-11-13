module Wizard.Database.Mapping.Questionnaire.QuestionnairePerm where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Questionnaire.QuestionnairePerm

instance ToField QuestionnairePermType where
  toField = toFieldGenericEnum

instance FromField QuestionnairePermType where
  fromField = fromFieldGenericEnum

instance ToRow QuestionnairePerm where
  toRow r@QuestionnairePerm {..} =
    [ toField questionnaireUuid
    , toField memberUuid
    , toField . PGArray $ perms
    , toField tenantUuid
    ]

instance FromRow QuestionnairePerm where
  fromRow = do
    questionnaireUuid <- field
    memberType <- field
    memberUuid <- field
    perms <- fromPGArray <$> field
    tenantUuid <- field
    return QuestionnairePerm {..}
