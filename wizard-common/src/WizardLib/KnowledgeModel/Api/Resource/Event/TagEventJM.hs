module WizardLib.KnowledgeModel.Api.Resource.Event.TagEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent

instance FromJSON AddTagEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddTagEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditTagEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditTagEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON DeleteTagEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteTagEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
