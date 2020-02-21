module Registry.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM where

import Data.Aeson

import Registry.Api.Resource.Event.EventJM ()
import Registry.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON KnowledgeModelChangeDTO where
  parseJSON = simpleParseJSON "_knowledgeModelChangeDTO"

instance ToJSON KnowledgeModelChangeDTO where
  toJSON = simpleToJSON "_knowledgeModelChangeDTO"
