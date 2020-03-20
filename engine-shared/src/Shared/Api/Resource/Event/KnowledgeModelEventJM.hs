module Shared.Api.Resource.Event.KnowledgeModelEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.KnowledgeModelEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_addKnowledgeModelEventDTO"

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "_addKnowledgeModelEventDTO" "eventType"

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_editKnowledgeModelEventDTO"

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "_editKnowledgeModelEventDTO" "eventType"
