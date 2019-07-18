module Database.BSON.Event.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.BSON.KnowledgeModel.Path ()
import LensesConfig
import Model.Event.KnowledgeModel.KnowledgeModelEvent

-- -------------------------------
-- ADD KNOWLEDGE MODEL EVENT -----
-- -------------------------------
instance ToBSON AddKnowledgeModelEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddKnowledgeModelEvent"
    , "uuid" BSON.=: (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "kmUuid" BSON.=: (event ^. kmUuid)
    , "name" BSON.=: (event ^. name)
    ]

instance FromBSON AddKnowledgeModelEvent where
  fromBSON doc = do
    kmUuid <- BSON.lookup "uuid" doc
    kmPath <- BSON.lookup "path" doc
    kmKmUuid <- BSON.lookup "kmUuid" doc
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
    , "uuid" BSON.=: (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "kmUuid" BSON.=: (event ^. kmUuid)
    , "name" BSON.=: (event ^. name)
    , "chapterUuids" BSON.=: (event ^. chapterUuids)
    , "tagUuids" BSON.=: (event ^. tagUuids)
    , "integrationUuids" BSON.=: (event ^. integrationUuids)
    ]

instance FromBSON EditKnowledgeModelEvent where
  fromBSON doc = do
    kmUuid <- BSON.lookup "uuid" doc
    kmPath <- BSON.lookup "path" doc
    kmKmUuid <- BSON.lookup "kmUuid" doc
    kmName <- BSON.lookup "name" doc
    kmChapterUuids <- BSON.lookup "chapterUuids" doc
    kmTagsUuids <- BSON.lookup "tagUuids" doc
    kmIntegrationUuids <- BSON.lookup "integrationUuids" doc
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
