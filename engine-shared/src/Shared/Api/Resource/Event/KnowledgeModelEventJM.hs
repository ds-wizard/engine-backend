module Shared.Api.Resource.Event.KnowledgeModelEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.JSON

instance FromJSON AddKnowledgeModelEvent where
  parseJSON = simpleParseJSON "_addKnowledgeModelEvent"

instance ToJSON AddKnowledgeModelEvent where
  toJSON = simpleToJSON' "_addKnowledgeModelEvent" "eventType"

instance FromJSON EditKnowledgeModelEvent where
  parseJSON = simpleParseJSON "_editKnowledgeModelEvent"

instance ToJSON EditKnowledgeModelEvent where
  toJSON = simpleToJSON' "_editKnowledgeModelEvent" "eventType"
