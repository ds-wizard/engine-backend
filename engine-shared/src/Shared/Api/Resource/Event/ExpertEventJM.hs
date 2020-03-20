module Shared.Api.Resource.Event.ExpertEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.ExpertEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddExpertEventDTO where
  parseJSON = simpleParseJSON "_addExpertEventDTO"

instance ToJSON AddExpertEventDTO where
  toJSON = simpleToJSON' "_addExpertEventDTO" "eventType"

instance FromJSON EditExpertEventDTO where
  parseJSON = simpleParseJSON "_editExpertEventDTO"

instance ToJSON EditExpertEventDTO where
  toJSON = simpleToJSON' "_editExpertEventDTO" "eventType"

instance FromJSON DeleteExpertEventDTO where
  parseJSON = simpleParseJSON "_deleteExpertEventDTO"

instance ToJSON DeleteExpertEventDTO where
  toJSON = simpleToJSON' "_deleteExpertEventDTO" "eventType"
