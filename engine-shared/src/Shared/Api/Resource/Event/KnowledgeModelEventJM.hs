module Shared.Api.Resource.Event.KnowledgeModelEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.KnowledgeModelEventDTO
import Shared.Util.JSON

instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "_addKnowledgeModelEventDTO" "eventType"

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "_editKnowledgeModelEventDTO" "eventType"
