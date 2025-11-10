module Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent

instance ToRow KnowledgeModelEditorRawEvent where
  toRow KnowledgeModelEditorRawEvent {..} =
    [ toField uuid
    , toField parentUuid
    , toField entityUuid
    , toJSONField content
    , toField knowledgeModelEditorUuid
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelEditorRawEvent where
  fromRow = do
    uuid <- field
    parentUuid <- field
    entityUuid <- field
    content <- fieldWith fromJSONField
    knowledgeModelEditorUuid <- field
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelEditorRawEvent {..}
