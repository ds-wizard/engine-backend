module Database.BSON.Event.EventPath where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import LensesConfig
import Model.Event.EventPath

instance ToBSON EventPathItem where
  toBSON value = ["type" BSON.=: value ^. pType, "uuid" BSON.=: serializeUUID (value ^. uuid)]

instance FromBSON EventPathItem where
  fromBSON doc = do
    epType <- BSON.lookup "type" doc
    epUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    return EventPathItem {_eventPathItemPType = epType, _eventPathItemUuid = epUuid}
