module Shared.Api.Resource.Event.PhaseEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Util.JSON

instance FromJSON AddPhaseEvent where
  parseJSON = simpleParseJSON "_addPhaseEvent"

instance ToJSON AddPhaseEvent where
  toJSON = simpleToJSON' "_addPhaseEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditPhaseEvent where
  parseJSON = simpleParseJSON "_editPhaseEvent"

instance ToJSON EditPhaseEvent where
  toJSON = simpleToJSON' "_editPhaseEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeletePhaseEvent where
  parseJSON = simpleParseJSON "_deletePhaseEvent"

instance ToJSON DeletePhaseEvent where
  toJSON = simpleToJSON' "_deletePhaseEvent" "eventType"
