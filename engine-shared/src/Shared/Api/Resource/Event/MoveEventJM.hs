module Shared.Api.Resource.Event.MoveEventJM where

import Data.Aeson

import Shared.Model.Event.Move.MoveEvent
import Shared.Util.Aeson

instance FromJSON MoveQuestionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveQuestionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveReferenceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveReferenceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
