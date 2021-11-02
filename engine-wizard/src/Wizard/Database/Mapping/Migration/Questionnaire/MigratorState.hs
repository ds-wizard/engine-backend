module Wizard.Database.Mapping.Migration.Questionnaire.MigratorState where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Migration.Questionnaire.MigratorState

instance ToRow MigratorState where
  toRow MigratorState {..} =
    [ toField _migratorStateOldQuestionnaireUuid
    , toField _migratorStateNewQuestionnaireUuid
    , toJSONField _migratorStateResolvedQuestionUuids
    , toField _migratorStateAppUuid
    ]

instance FromRow MigratorState where
  fromRow = do
    _migratorStateOldQuestionnaireUuid <- field
    _migratorStateNewQuestionnaireUuid <- field
    _migratorStateResolvedQuestionUuids <- fieldWith fromJSONField
    _migratorStateAppUuid <- field
    return $ MigratorState {..}
