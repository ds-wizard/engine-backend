module Shared.Api.Resource.Event.ChoiceEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Util.JSON

instance FromJSON AddChoiceEvent where
  parseJSON = simpleParseJSON "_addChoiceEvent"

instance ToJSON AddChoiceEvent where
  toJSON = simpleToJSON' "_addChoiceEvent" "eventType"

instance FromJSON EditChoiceEvent where
  parseJSON = simpleParseJSON "_editChoiceEvent"

instance ToJSON EditChoiceEvent where
  toJSON = simpleToJSON' "_editChoiceEvent" "eventType"

instance FromJSON DeleteChoiceEvent where
  parseJSON = simpleParseJSON "_deleteChoiceEvent"

instance ToJSON DeleteChoiceEvent where
  toJSON = simpleToJSON' "_deleteChoiceEvent" "eventType"
