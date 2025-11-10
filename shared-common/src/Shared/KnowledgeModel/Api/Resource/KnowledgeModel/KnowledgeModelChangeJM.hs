module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO

instance FromJSON KnowledgeModelChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelChangeDTO where
  toJSON = genericToJSON jsonOptions
