module Wizard.Database.BSON.Questionnaire.QuestionnaireEvent where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Wizard.Database.BSON.Questionnaire.QuestionnaireReply ()
import Wizard.Model.Questionnaire.QuestionnaireEvent

instance FromBSON QuestionnaireEvent where
  fromBSON doc = do
    eventType <- BSON.lookup "type" doc
    case eventType of
      "SetReplyEvent" -> Just . SetReplyEvent' . fromJust $ (fromBSON doc :: Maybe SetReplyEvent)
      "ClearReplyEvent" -> Just . ClearReplyEvent' . fromJust $ (fromBSON doc :: Maybe ClearReplyEvent)
      "SetLevelEvent" -> Just . SetLevelEvent' . fromJust $ (fromBSON doc :: Maybe SetLevelEvent)
      "SetLabelsEvent" -> Just . SetLabelsEvent' . fromJust $ (fromBSON doc :: Maybe SetLabelsEvent)

instance ToBSON QuestionnaireEvent where
  toBSON (SetReplyEvent' event) = toBSON event
  toBSON (ClearReplyEvent' event) = toBSON event
  toBSON (SetLevelEvent' event) = toBSON event
  toBSON (SetLabelsEvent' event) = toBSON event

-- ------------------------------------------------------------------------
instance ToBSON SetReplyEvent where
  toBSON SetReplyEvent {..} =
    [ "type" BSON.=: "SetReplyEvent"
    , "uuid" BSON.=: _setReplyEventUuid
    , "path" BSON.=: _setReplyEventPath
    , "value" BSON.=: _setReplyEventValue
    , "createdBy" BSON.=: _setReplyEventCreatedBy
    , "createdAt" BSON.=: _setReplyEventCreatedAt
    ]

instance FromBSON SetReplyEvent

-- ------------------------------------------------------------------------
instance ToBSON ClearReplyEvent where
  toBSON ClearReplyEvent {..} =
    [ "type" BSON.=: "ClearReplyEvent"
    , "uuid" BSON.=: _clearReplyEventUuid
    , "path" BSON.=: _clearReplyEventPath
    , "createdBy" BSON.=: _clearReplyEventCreatedBy
    , "createdAt" BSON.=: _clearReplyEventCreatedAt
    ]

instance FromBSON ClearReplyEvent

-- ------------------------------------------------------------------------
instance ToBSON SetLevelEvent where
  toBSON SetLevelEvent {..} =
    [ "type" BSON.=: "SetLevelEvent"
    , "uuid" BSON.=: _setLevelEventUuid
    , "level" BSON.=: _setLevelEventLevel
    , "createdBy" BSON.=: _setLevelEventCreatedBy
    , "createdAt" BSON.=: _setLevelEventCreatedAt
    ]

instance FromBSON SetLevelEvent

-- ------------------------------------------------------------------------
instance ToBSON SetLabelsEvent where
  toBSON SetLabelsEvent {..} =
    [ "type" BSON.=: "SetLabelsEvent"
    , "uuid" BSON.=: _setLabelsEventUuid
    , "path" BSON.=: _setLabelsEventPath
    , "value" BSON.=: _setLabelsEventValue
    , "createdBy" BSON.=: _setLabelsEventCreatedBy
    , "createdAt" BSON.=: _setLabelsEventCreatedAt
    ]

instance FromBSON SetLabelsEvent
