module Shared.Database.BSON.Event.Choice where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Event.EventField ()
import Shared.Model.Event.Choice.ChoiceEvent

-- -------------------------
-- ADD ANSWER EVENT --------
-- -------------------------
instance ToBSON AddChoiceEvent where
  toBSON AddChoiceEvent {..} =
    [ "eventType" BSON.=: "AddChoiceEvent"
    , "uuid" BSON.=: _addChoiceEventUuid
    , "parentUuid" BSON.=: _addChoiceEventParentUuid
    , "entityUuid" BSON.=: _addChoiceEventEntityUuid
    , "label" BSON.=: _addChoiceEventLabel
    ]

instance FromBSON AddChoiceEvent where
  fromBSON doc = do
    _addChoiceEventUuid <- BSON.lookup "uuid" doc
    _addChoiceEventParentUuid <- BSON.lookup "parentUuid" doc
    _addChoiceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addChoiceEventLabel <- BSON.lookup "label" doc
    return AddChoiceEvent {..}

-- -------------------------
-- EDIT ANSWER EVENT -------
-- -------------------------
instance ToBSON EditChoiceEvent where
  toBSON EditChoiceEvent {..} =
    [ "eventType" BSON.=: "EditChoiceEvent"
    , "uuid" BSON.=: _editChoiceEventUuid
    , "parentUuid" BSON.=: _editChoiceEventParentUuid
    , "entityUuid" BSON.=: _editChoiceEventEntityUuid
    , "label" BSON.=: _editChoiceEventLabel
    ]

instance FromBSON EditChoiceEvent where
  fromBSON doc = do
    _editChoiceEventUuid <- BSON.lookup "uuid" doc
    _editChoiceEventParentUuid <- BSON.lookup "parentUuid" doc
    _editChoiceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editChoiceEventLabel <- BSON.lookup "label" doc
    return EditChoiceEvent {..}

-- -------------------------
-- DELETE ANSWER EVENT -----
-- -------------------------
instance ToBSON DeleteChoiceEvent where
  toBSON DeleteChoiceEvent {..} =
    [ "eventType" BSON.=: "DeleteChoiceEvent"
    , "uuid" BSON.=: _deleteChoiceEventUuid
    , "parentUuid" BSON.=: _deleteChoiceEventParentUuid
    , "entityUuid" BSON.=: _deleteChoiceEventEntityUuid
    ]

instance FromBSON DeleteChoiceEvent where
  fromBSON doc = do
    _deleteChoiceEventUuid <- BSON.lookup "uuid" doc
    _deleteChoiceEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteChoiceEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteChoiceEvent {..}
