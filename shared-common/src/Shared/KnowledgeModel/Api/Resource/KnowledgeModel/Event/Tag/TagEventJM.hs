module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Tag.TagEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent

instance FromJSON AddTagEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddTagEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditTagEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditTagEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
