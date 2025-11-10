module Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorEvent where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent

instance ToRow KnowledgeModelEditorEvent where
  toRow KnowledgeModelEditorEvent {..} =
    [ toField uuid
    , toField parentUuid
    , toField entityUuid
    , toJSONField content
    , toField knowledgeModelEditorUuid
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelEditorEvent where
  fromRow = do
    uuid <- field
    parentUuid <- field
    entityUuid <- field
    content <- fieldWith fromJSONField
    knowledgeModelEditorUuid <- field
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelEditorEvent {..}
