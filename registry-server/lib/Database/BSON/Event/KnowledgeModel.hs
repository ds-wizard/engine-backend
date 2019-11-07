module Database.BSON.Event.KnowledgeModel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Model.Event.KnowledgeModel.KnowledgeModelEvent

-- -------------------------------
-- ADD KNOWLEDGE MODEL EVENT -----
-- -------------------------------
instance ToBSON AddKnowledgeModelEvent where
  toBSON AddKnowledgeModelEvent {..} =
    [ "eventType" BSON.=: "AddKnowledgeModelEvent"
    , "uuid" BSON.=: _addKnowledgeModelEventUuid
    , "parentUuid" BSON.=: _addKnowledgeModelEventParentUuid
    , "entityUuid" BSON.=: _addKnowledgeModelEventEntityUuid
    , "name" BSON.=: _addKnowledgeModelEventName
    ]

instance FromBSON AddKnowledgeModelEvent where
  fromBSON doc = do
    _addKnowledgeModelEventUuid <- BSON.lookup "uuid" doc
    _addKnowledgeModelEventParentUuid <- BSON.lookup "parentUuid" doc
    _addKnowledgeModelEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addKnowledgeModelEventName <- BSON.lookup "name" doc
    return AddKnowledgeModelEvent {..}

-- -------------------------------
-- EDIT KNOWLEDGE MODEL EVENT ----
-- -------------------------------
instance ToBSON EditKnowledgeModelEvent where
  toBSON EditKnowledgeModelEvent {..} =
    [ "eventType" BSON.=: "EditKnowledgeModelEvent"
    , "uuid" BSON.=: _editKnowledgeModelEventUuid
    , "parentUuid" BSON.=: _editKnowledgeModelEventParentUuid
    , "entityUuid" BSON.=: _editKnowledgeModelEventEntityUuid
    , "name" BSON.=: _editKnowledgeModelEventName
    , "chapterUuids" BSON.=: _editKnowledgeModelEventChapterUuids
    , "tagUuids" BSON.=: _editKnowledgeModelEventTagUuids
    , "integrationUuids" BSON.=: _editKnowledgeModelEventIntegrationUuids
    ]

instance FromBSON EditKnowledgeModelEvent where
  fromBSON doc = do
    _editKnowledgeModelEventUuid <- BSON.lookup "uuid" doc
    _editKnowledgeModelEventParentUuid <- BSON.lookup "parentUuid" doc
    _editKnowledgeModelEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editKnowledgeModelEventName <- BSON.lookup "name" doc
    _editKnowledgeModelEventChapterUuids <- BSON.lookup "chapterUuids" doc
    _editKnowledgeModelEventTagUuids <- BSON.lookup "tagUuids" doc
    _editKnowledgeModelEventIntegrationUuids <- BSON.lookup "integrationUuids" doc
    return EditKnowledgeModelEvent {..}
