module Shared.Api.Resource.Event.MoveEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.MoveEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON MoveQuestionEventDTO where
  parseJSON = simpleParseJSON "_moveQuestionEventDTO"

instance ToJSON MoveQuestionEventDTO where
  toJSON = simpleToJSON' "_moveQuestionEventDTO" "eventType"

instance FromJSON MoveAnswerEventDTO where
  parseJSON = simpleParseJSON "_moveAnswerEventDTO"

instance ToJSON MoveAnswerEventDTO where
  toJSON = simpleToJSON' "_moveAnswerEventDTO" "eventType"

instance FromJSON MoveExpertEventDTO where
  parseJSON = simpleParseJSON "_moveExpertEventDTO"

instance ToJSON MoveExpertEventDTO where
  toJSON = simpleToJSON' "_moveExpertEventDTO" "eventType"

instance FromJSON MoveReferenceEventDTO where
  parseJSON = simpleParseJSON "_moveReferenceEventDTO"

instance ToJSON MoveReferenceEventDTO where
  toJSON = simpleToJSON' "_moveReferenceEventDTO" "eventType"
