module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelRawEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelRawEvent

instance ToJSON KnowledgeModelRawEvent where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelRawEvent where
  parseJSON = genericParseJSON jsonOptions
