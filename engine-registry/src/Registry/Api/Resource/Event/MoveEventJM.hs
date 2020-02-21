module Registry.Api.Resource.Event.MoveEventJM where

import Data.Aeson

import Registry.Api.Resource.Event.MoveEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON MoveQuestionEventDTO where
  parseJSON = simpleParseJSON "_moveQuestionEventDTO"

instance ToJSON MoveQuestionEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveQuestionEventDTO"

instance FromJSON MoveAnswerEventDTO where
  parseJSON = simpleParseJSON "_moveAnswerEventDTO"

instance ToJSON MoveAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveAnswerEventDTO"

instance FromJSON MoveExpertEventDTO where
  parseJSON = simpleParseJSON "_moveExpertEventDTO"

instance ToJSON MoveExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveExpertEventDTO"

instance FromJSON MoveReferenceEventDTO where
  parseJSON = simpleParseJSON "_moveReferenceEventDTO"

instance ToJSON MoveReferenceEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveReferenceEventDTO"
