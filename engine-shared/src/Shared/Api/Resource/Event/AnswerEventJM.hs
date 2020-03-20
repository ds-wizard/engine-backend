module Shared.Api.Resource.Event.AnswerEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.AnswerEventDTO
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddAnswerEventDTO where
  parseJSON = simpleParseJSON "_addAnswerEventDTO"

instance ToJSON AddAnswerEventDTO where
  toJSON = simpleToJSON' "_addAnswerEventDTO" "eventType"

instance FromJSON EditAnswerEventDTO where
  parseJSON = simpleParseJSON "_editAnswerEventDTO"

instance ToJSON EditAnswerEventDTO where
  toJSON = simpleToJSON' "_editAnswerEventDTO" "eventType"

instance FromJSON DeleteAnswerEventDTO where
  parseJSON = simpleParseJSON "_deleteAnswerEventDTO"

instance ToJSON DeleteAnswerEventDTO where
  toJSON = simpleToJSON' "_deleteAnswerEventDTO" "eventType"
