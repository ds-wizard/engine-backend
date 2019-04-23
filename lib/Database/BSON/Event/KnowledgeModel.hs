module Database.BSON.Event.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.Event.EventPath ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import LensesConfig
import Model.Event.KnowledgeModel.KnowledgeModelEvent

-- -------------------------------
-- ADD KNOWLEDGE MODEL EVENT -----
-- -------------------------------
instance ToBSON AddKnowledgeModelEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddKnowledgeModelEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "name" BSON.=: (event ^. name)
    ]

instance FromBSON AddKnowledgeModelEvent where
  fromBSON doc = do
    kmUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    kmPath <- BSON.lookup "path" doc
    kmKmUuid <- deserializeMaybeUUID $ BSON.lookup "kmUuid" doc
    kmName <- BSON.lookup "name" doc
    return
      AddKnowledgeModelEvent
      { _addKnowledgeModelEventUuid = kmUuid
      , _addKnowledgeModelEventPath = kmPath
      , _addKnowledgeModelEventKmUuid = kmKmUuid
      , _addKnowledgeModelEventName = kmName
      }

-- -------------------------------
-- EDIT KNOWLEDGE MODEL EVENT ----
-- -------------------------------
instance ToBSON EditKnowledgeModelEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditKnowledgeModelEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "name" BSON.=: (event ^. name)
    , "chapterUuids" BSON.=: serializeEventFieldUUIDList (event ^. chapterUuids)
    , "tagUuids" BSON.=: serializeEventFieldUUIDList (event ^. tagUuids)
    , "integrationUuids" BSON.=: serializeEventFieldUUIDList (event ^. integrationUuids)
    ]

instance FromBSON EditKnowledgeModelEvent where
  fromBSON doc = do
    kmUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    kmPath <- BSON.lookup "path" doc
    kmKmUuid <- deserializeMaybeUUID $ BSON.lookup "kmUuid" doc
    kmName <- BSON.lookup "name" doc
    let kmChapterUuids = deserializeEventFieldUUIDList $ BSON.lookup "chapterUuids" doc
    let kmTagsUuids = deserializeEventFieldUUIDList $ BSON.lookup "tagUuids" doc
    let kmIntegrationUuids = deserializeEventFieldUUIDList $ BSON.lookup "integrationUuids" doc
    return
      EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid = kmUuid
      , _editKnowledgeModelEventPath = kmPath
      , _editKnowledgeModelEventKmUuid = kmKmUuid
      , _editKnowledgeModelEventName = kmName
      , _editKnowledgeModelEventChapterUuids = kmChapterUuids
      , _editKnowledgeModelEventTagUuids = kmTagsUuids
      , _editKnowledgeModelEventIntegrationUuids = kmIntegrationUuids
      }
