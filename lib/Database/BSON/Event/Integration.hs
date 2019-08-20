module Database.BSON.Event.Integration where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Model.Event.Integration.IntegrationEvent

-- -------------------------
-- ADD TAG EVENT -----------
-- -------------------------
instance ToBSON AddIntegrationEvent where
  toBSON AddIntegrationEvent {..} =
    [ "eventType" BSON.=: "AddIntegrationEvent"
    , "uuid" BSON.=: _addIntegrationEventUuid
    , "parentUuid" BSON.=: _addIntegrationEventParentUuid
    , "entityUuid" BSON.=: _addIntegrationEventEntityUuid
    , "id" BSON.=: _addIntegrationEventIId
    , "name" BSON.=: _addIntegrationEventName
    , "props" BSON.=: _addIntegrationEventProps
    , "logo" BSON.=: _addIntegrationEventLogo
    , "requestMethod" BSON.=: _addIntegrationEventRequestMethod
    , "requestUrl" BSON.=: _addIntegrationEventRequestUrl
    , "requestHeaders" BSON.=: _addIntegrationEventRequestHeaders
    , "requestBody" BSON.=: _addIntegrationEventRequestBody
    , "responseListField" BSON.=: _addIntegrationEventResponseListField
    , "responseIdField" BSON.=: _addIntegrationEventResponseIdField
    , "responseNameField" BSON.=: _addIntegrationEventResponseNameField
    , "itemUrl" BSON.=: _addIntegrationEventItemUrl
    ]

instance FromBSON AddIntegrationEvent where
  fromBSON doc = do
    _addIntegrationEventUuid <- BSON.lookup "uuid" doc
    _addIntegrationEventParentUuid <- BSON.lookup "parentUuid" doc
    _addIntegrationEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addIntegrationEventIId <- BSON.lookup "id" doc
    _addIntegrationEventName <- BSON.lookup "name" doc
    _addIntegrationEventProps <- BSON.lookup "props" doc
    _addIntegrationEventLogo <- BSON.lookup "logo" doc
    _addIntegrationEventRequestMethod <- BSON.lookup "requestMethod" doc
    _addIntegrationEventRequestUrl <- BSON.lookup "requestUrl" doc
    _addIntegrationEventRequestHeaders <- BSON.lookup "requestHeaders" doc
    _addIntegrationEventRequestBody <- BSON.lookup "requestBody" doc
    _addIntegrationEventResponseListField <- BSON.lookup "responseListField" doc
    _addIntegrationEventResponseIdField <- BSON.lookup "responseIdField" doc
    _addIntegrationEventResponseNameField <- BSON.lookup "responseNameField" doc
    _addIntegrationEventItemUrl <- BSON.lookup "itemUrl" doc
    return AddIntegrationEvent {..}

-- -------------------------
-- EDIT TAG EVENT ----------
-- -------------------------
instance ToBSON EditIntegrationEvent where
  toBSON EditIntegrationEvent {..} =
    [ "eventType" BSON.=: "EditIntegrationEvent"
    , "uuid" BSON.=: _editIntegrationEventUuid
    , "parentUuid" BSON.=: _editIntegrationEventParentUuid
    , "entityUuid" BSON.=: _editIntegrationEventEntityUuid
    , "id" BSON.=: _editIntegrationEventIId
    , "name" BSON.=: _editIntegrationEventName
    , "props" BSON.=: _editIntegrationEventProps
    , "logo" BSON.=: _editIntegrationEventLogo
    , "requestMethod" BSON.=: _editIntegrationEventRequestMethod
    , "requestUrl" BSON.=: _editIntegrationEventRequestUrl
    , "requestHeaders" BSON.=: _editIntegrationEventRequestHeaders
    , "requestBody" BSON.=: _editIntegrationEventRequestBody
    , "responseListField" BSON.=: _editIntegrationEventResponseListField
    , "responseIdField" BSON.=: _editIntegrationEventResponseIdField
    , "responseNameField" BSON.=: _editIntegrationEventResponseNameField
    , "itemUrl" BSON.=: _editIntegrationEventItemUrl
    ]

instance FromBSON EditIntegrationEvent where
  fromBSON doc = do
    _editIntegrationEventUuid <- BSON.lookup "uuid" doc
    _editIntegrationEventParentUuid <- BSON.lookup "parentUuid" doc
    _editIntegrationEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editIntegrationEventIId <- BSON.lookup "id" doc
    _editIntegrationEventName <- BSON.lookup "name" doc
    _editIntegrationEventProps <- BSON.lookup "props" doc
    _editIntegrationEventLogo <- BSON.lookup "logo" doc
    _editIntegrationEventRequestMethod <- BSON.lookup "requestMethod" doc
    _editIntegrationEventRequestUrl <- BSON.lookup "requestUrl" doc
    _editIntegrationEventRequestHeaders <- BSON.lookup "requestHeaders" doc
    _editIntegrationEventRequestBody <- BSON.lookup "requestBody" doc
    _editIntegrationEventResponseListField <- BSON.lookup "responseListField" doc
    _editIntegrationEventResponseIdField <- BSON.lookup "responseIdField" doc
    _editIntegrationEventResponseNameField <- BSON.lookup "responseNameField" doc
    _editIntegrationEventItemUrl <- BSON.lookup "itemUrl" doc
    return EditIntegrationEvent {..}

-- -------------------------
-- DELETE TAG EVENT --------
-- -------------------------
instance ToBSON DeleteIntegrationEvent where
  toBSON DeleteIntegrationEvent {..} =
    [ "eventType" BSON.=: "DeleteIntegrationEvent"
    , "uuid" BSON.=: _deleteIntegrationEventUuid
    , "parentUuid" BSON.=: _deleteIntegrationEventParentUuid
    , "entityUuid" BSON.=: _deleteIntegrationEventEntityUuid
    ]

instance FromBSON DeleteIntegrationEvent where
  fromBSON doc = do
    _deleteIntegrationEventUuid <- BSON.lookup "uuid" doc
    _deleteIntegrationEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteIntegrationEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteIntegrationEvent {..}
