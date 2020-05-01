module Shared.Api.Resource.Event.MoveEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.MoveEventDTO
import Shared.Util.JSON

instance FromJSON MoveQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MoveQuestionEventDTO where
  toJSON = simpleToJSON' "_moveQuestionEventDTO" "eventType"

instance FromJSON MoveAnswerEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MoveAnswerEventDTO where
  toJSON = simpleToJSON' "_moveAnswerEventDTO" "eventType"

instance FromJSON MoveExpertEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MoveExpertEventDTO where
  toJSON = simpleToJSON' "_moveExpertEventDTO" "eventType"

instance FromJSON MoveReferenceEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MoveReferenceEventDTO where
  toJSON = simpleToJSON' "_moveReferenceEventDTO" "eventType"
