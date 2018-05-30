module Database.BSON.Event.Reference where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.UUID

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.Event.EventPath ()
import LensesConfig
import Model.Event.Reference.ReferenceEvent

-- -------------------------
-- ADD REFERNCE EVENT ------
-- -------------------------
instance ToBSON AddReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "referenceUuid" BSON.=: serializeUUID (event ^. referenceUuid)
    , "chapter" BSON.=: (event ^. chapter)
    ]

instance FromBSON AddReferenceEvent where
  fromBSON doc = do
    refUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    refChapter <- BSON.lookup "chapter" doc
    return
      AddReferenceEvent
      { _addReferenceEventUuid = refUuid
      , _addReferenceEventPath = refPath
      , _addReferenceEventReferenceUuid = refReferenceUuid
      , _addReferenceEventChapter = refChapter
      }

-- -------------------------
-- EDIT REFERNCE EVENT -----
-- -------------------------
instance ToBSON EditReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "referenceUuid" BSON.=: serializeUUID (event ^. referenceUuid)
    , "chapter" BSON.=: (event ^. chapter)
    ]

instance FromBSON EditReferenceEvent where
  fromBSON doc = do
    refUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    refChapter <- BSON.lookup "chapter" doc
    return
      EditReferenceEvent
      { _editReferenceEventUuid = refUuid
      , _editReferenceEventPath = refPath
      , _editReferenceEventReferenceUuid = refReferenceUuid
      , _editReferenceEventChapter = refChapter
      }

-- -------------------------
-- DELETE REFERNCE EVENT ---
-- -------------------------
instance ToBSON DeleteReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "referenceUuid" BSON.=: serializeUUID (event ^. referenceUuid)
    ]

instance FromBSON DeleteReferenceEvent where
  fromBSON doc = do
    refUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    return
      DeleteReferenceEvent
      { _deleteReferenceEventUuid = refUuid
      , _deleteReferenceEventPath = refPath
      , _deleteReferenceEventReferenceUuid = refReferenceUuid
      }
