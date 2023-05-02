module WizardLib.KnowledgeModel.Api.Resource.Event.ChoiceEventJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent

instance FromJSON AddChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON DeleteChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
