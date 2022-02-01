module Shared.Api.Resource.Event.ExpertEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Util.JSON

instance FromJSON AddExpertEvent where
  parseJSON = simpleParseJSON "_addExpertEvent"

instance ToJSON AddExpertEvent where
  toJSON = simpleToJSON' "_addExpertEvent" "eventType"

instance FromJSON EditExpertEvent where
  parseJSON = simpleParseJSON "_editExpertEvent"

instance ToJSON EditExpertEvent where
  toJSON = simpleToJSON' "_editExpertEvent" "eventType"

instance FromJSON DeleteExpertEvent where
  parseJSON = simpleParseJSON "_deleteExpertEvent"

instance ToJSON DeleteExpertEvent where
  toJSON = simpleToJSON' "_deleteExpertEvent" "eventType"
