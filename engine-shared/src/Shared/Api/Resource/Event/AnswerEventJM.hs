module Shared.Api.Resource.Event.AnswerEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Util.JSON

instance FromJSON AddAnswerEvent where
  parseJSON = simpleParseJSON "_addAnswerEvent"

instance ToJSON AddAnswerEvent where
  toJSON = simpleToJSON' "_addAnswerEvent" "eventType"

instance FromJSON EditAnswerEvent where
  parseJSON = simpleParseJSON "_editAnswerEvent"

instance ToJSON EditAnswerEvent where
  toJSON = simpleToJSON' "_editAnswerEvent" "eventType"

instance FromJSON DeleteAnswerEvent where
  parseJSON = simpleParseJSON "_deleteAnswerEvent"

instance ToJSON DeleteAnswerEvent where
  toJSON = simpleToJSON' "_deleteAnswerEvent" "eventType"
