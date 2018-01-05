module Database.BSON.Event.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent

-- -------------------------------
-- ADD KNOWLEDGE MODEL EVENT -----
-- -------------------------------
instance ToBSON AddKnowledgeModelEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddKnowledgeModelEvent"
    , "uuid" BSON.=: serializeUUID (event ^. akmUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. akmKmUuid)
    , "name" BSON.=: (event ^. akmName)
    ]

instance FromBSON AddKnowledgeModelEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    name <- BSON.lookup "name" doc
    return AddKnowledgeModelEvent {_akmUuid = uuid, _akmKmUuid = kmUuid, _akmName = name}

-- -------------------------------
-- EDIT KNOWLEDGE MODEL EVENT ----
-- -------------------------------
instance ToBSON EditKnowledgeModelEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditKnowledgeModelEvent"
    , "uuid" BSON.=: serializeUUID (event ^. ekmUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. ekmKmUuid)
    , "name" BSON.=: (event ^. ekmName)
    , "chapterIds" BSON.=: serializeMaybeUUIDList (event ^. ekmChapterIds)
    ]

instance FromBSON EditKnowledgeModelEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    name <- BSON.lookup "name" doc
    let chapterIds = deserializeMaybeUUIDList $ BSON.lookup "chapterIds" doc
    return EditKnowledgeModelEvent {_ekmUuid = uuid, _ekmKmUuid = kmUuid, _ekmName = name, _ekmChapterIds = chapterIds}
