module Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM where

import Data.Aeson

import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON KnowledgeModelChangeDTO where
  parseJSON = simpleParseJSON "_knowledgeModelChangeDTO"

instance ToJSON KnowledgeModelChangeDTO where
  toJSON = simpleToJSON "_knowledgeModelChangeDTO"
