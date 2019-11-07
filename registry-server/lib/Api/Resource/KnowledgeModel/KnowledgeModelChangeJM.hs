module Api.Resource.KnowledgeModel.KnowledgeModelChangeJM where

import Data.Aeson

import Api.Resource.Event.EventJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON KnowledgeModelChangeDTO where
  parseJSON = simpleParseJSON "_knowledgeModelChangeDTO"

instance ToJSON KnowledgeModelChangeDTO where
  toJSON = simpleToJSON "_knowledgeModelChangeDTO"
