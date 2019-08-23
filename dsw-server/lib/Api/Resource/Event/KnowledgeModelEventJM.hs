module Api.Resource.Event.KnowledgeModelEventJM where

import Data.Aeson

import Api.Resource.Event.EventFieldJM ()
import Api.Resource.Event.KnowledgeModelEventDTO
import Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_addKnowledgeModelEventDTO"

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "eventType" "_addKnowledgeModelEventDTO"

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_editKnowledgeModelEventDTO"

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "eventType" "_editKnowledgeModelEventDTO"
