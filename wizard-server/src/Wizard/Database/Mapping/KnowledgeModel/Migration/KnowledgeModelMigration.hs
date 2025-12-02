module Wizard.Database.Mapping.KnowledgeModel.Migration.KnowledgeModelMigration where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

instance ToRow KnowledgeModelMigration where
  toRow KnowledgeModelMigration {..} =
    [ toField editorUuid
    , toField metamodelVersion
    , toJSONField state
    , toField editorPreviousPackageId
    , toField targetPackageId
    , toJSONField editorPreviousPackageEvents
    , toJSONField targetPackageEvents
    , toJSONField resultEvents
    , toJSONField currentKnowledgeModel
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelMigration where
  fromRow = do
    editorUuid <- field
    metamodelVersion <- field
    state <- fieldWith fromJSONField
    editorPreviousPackageId <- field
    targetPackageId <- field
    editorPreviousPackageEvents <- fieldWith fromJSONField
    targetPackageEvents <- fieldWith fromJSONField
    resultEvents <- fieldWith fromJSONField
    currentKnowledgeModel <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelMigration {..}
