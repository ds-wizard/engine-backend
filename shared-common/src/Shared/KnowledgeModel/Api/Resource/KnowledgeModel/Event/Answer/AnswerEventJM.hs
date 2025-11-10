module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent

instance FromJSON AddAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
