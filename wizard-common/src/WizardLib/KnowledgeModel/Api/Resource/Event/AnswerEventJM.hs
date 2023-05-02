module WizardLib.KnowledgeModel.Api.Resource.Event.AnswerEventJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent

instance FromJSON AddAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON DeleteAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
