module Wizard.Database.Mapping.Questionnaire.MigratorState where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Model.Questionnaire.MigratorState

instance ToRow MigratorState where
  toRow MigratorState {..} =
    [ toField oldQuestionnaireUuid
    , toField newQuestionnaireUuid
    , toField . PGArray $ resolvedQuestionUuids
    , toField tenantUuid
    ]

instance FromRow MigratorState where
  fromRow = do
    oldQuestionnaireUuid <- field
    newQuestionnaireUuid <- field
    resolvedQuestionUuids <- fromPGArray <$> field
    tenantUuid <- field
    return $ MigratorState {..}
