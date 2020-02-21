module Registry.Api.Resource.Event.AnswerEventJM where

import Data.Aeson

import Registry.Api.Resource.Event.AnswerEventDTO
import Registry.Api.Resource.Event.EventFieldJM ()
import Registry.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddAnswerEventDTO where
  parseJSON = simpleParseJSON "_addAnswerEventDTO"

instance ToJSON AddAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_addAnswerEventDTO"

instance FromJSON EditAnswerEventDTO where
  parseJSON = simpleParseJSON "_editAnswerEventDTO"

instance ToJSON EditAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_editAnswerEventDTO"

instance FromJSON DeleteAnswerEventDTO where
  parseJSON = simpleParseJSON "_deleteAnswerEventDTO"

instance ToJSON DeleteAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteAnswerEventDTO"
