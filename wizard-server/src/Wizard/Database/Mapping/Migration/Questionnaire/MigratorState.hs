module Wizard.Database.Mapping.Migration.Questionnaire.MigratorState where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Migration.Questionnaire.MigratorState

instance ToRow MigratorState where
  toRow MigratorState {..} =
    [ toField oldQuestionnaireUuid
    , toField newQuestionnaireUuid
    , toJSONField resolvedQuestionUuids
    , toField tenantUuid
    ]

instance FromRow MigratorState where
  fromRow = do
    oldQuestionnaireUuid <- field
    newQuestionnaireUuid <- field
    resolvedQuestionUuids <- fieldWith fromJSONField
    tenantUuid <- field
    return $ MigratorState {..}
