module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Util.Aeson

instance FromJSON KnowledgeModelChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelChangeDTO where
  toJSON = genericToJSON jsonOptions
