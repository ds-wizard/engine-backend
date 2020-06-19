module Shared.Api.Resource.Event.MoveEventJM where

import Data.Aeson

import Shared.Model.Event.Move.MoveEvent
import Shared.Util.JSON

instance FromJSON MoveQuestionEvent where
  parseJSON = simpleParseJSON "_moveQuestionEvent"

instance ToJSON MoveQuestionEvent where
  toJSON = simpleToJSON' "_moveQuestionEvent" "eventType"

instance FromJSON MoveAnswerEvent where
  parseJSON = simpleParseJSON "_moveAnswerEvent"

instance ToJSON MoveAnswerEvent where
  toJSON = simpleToJSON' "_moveAnswerEvent" "eventType"

instance FromJSON MoveExpertEvent where
  parseJSON = simpleParseJSON "_moveExpertEvent"

instance ToJSON MoveExpertEvent where
  toJSON = simpleToJSON' "_moveExpertEvent" "eventType"

instance FromJSON MoveReferenceEvent where
  parseJSON = simpleParseJSON "_moveReferenceEvent"

instance ToJSON MoveReferenceEvent where
  toJSON = simpleToJSON' "_moveReferenceEvent" "eventType"
