module WizardLib.KnowledgeModel.Api.Resource.Event.KnowledgeModelEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent

instance FromJSON AddKnowledgeModelEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddKnowledgeModelEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditKnowledgeModelEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditKnowledgeModelEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
