module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance ToJSON QuestionnaireEventDTO where
  toJSON = toSumJSON' "type"

instance FromJSON QuestionnaireEventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEventDTO' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEventDTO' event)
      "SetLevelEvent" -> parseJSON (Object o) >>= \event -> return (SetLevelEventDTO' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEventDTO' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON SetReplyEventDTO where
  parseJSON = simpleParseJSON "_setReplyEventDTO"

instance ToJSON SetReplyEventDTO where
  toJSON = simpleToJSON' "_setReplyEventDTO" "type"

instance FromJSON ClearReplyEventDTO where
  parseJSON = simpleParseJSON "_clearReplyEventDTO"

instance ToJSON ClearReplyEventDTO where
  toJSON = simpleToJSON' "_clearReplyEventDTO" "type"

instance FromJSON SetLevelEventDTO where
  parseJSON = simpleParseJSON "_setLevelEventDTO"

instance ToJSON SetLevelEventDTO where
  toJSON = simpleToJSON' "_setLevelEventDTO" "type"

instance FromJSON SetLabelsEventDTO where
  parseJSON = simpleParseJSON "_setLabelsEventDTO"

instance ToJSON SetLabelsEventDTO where
  toJSON = simpleToJSON' "_setLabelsEventDTO" "type"
