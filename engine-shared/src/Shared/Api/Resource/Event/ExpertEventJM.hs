module Shared.Api.Resource.Event.ExpertEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.ExpertEventDTO
import Shared.Util.JSON

instance FromJSON AddExpertEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddExpertEventDTO where
  toJSON = simpleToJSON' "_addExpertEventDTO" "eventType"

instance FromJSON EditExpertEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditExpertEventDTO where
  toJSON = simpleToJSON' "_editExpertEventDTO" "eventType"

instance FromJSON DeleteExpertEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DeleteExpertEventDTO where
  toJSON = simpleToJSON' "_deleteExpertEventDTO" "eventType"
