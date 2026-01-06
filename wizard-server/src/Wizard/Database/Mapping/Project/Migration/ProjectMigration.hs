module Wizard.Database.Mapping.Project.Migration.ProjectMigration where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Model.Project.Migration.ProjectMigration

instance ToRow ProjectMigration where
  toRow ProjectMigration {..} =
    [ toField oldProjectUuid
    , toField newProjectUuid
    , toField . PGArray $ resolvedQuestionUuids
    , toField tenantUuid
    ]

instance FromRow ProjectMigration where
  fromRow = do
    oldProjectUuid <- field
    newProjectUuid <- field
    resolvedQuestionUuids <- fromPGArray <$> field
    tenantUuid <- field
    return $ ProjectMigration {..}
