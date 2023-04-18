module WizardLib.KnowledgeModel.Api.Resource.Event.ExpertEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent

instance FromJSON AddExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON DeleteExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
