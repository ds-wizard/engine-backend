module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance ToJSON QuestionnaireEventChangeDTO where
  toJSON = toSumJSON'' "type" "ChangeDTO"

instance FromJSON QuestionnaireEventChangeDTO where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEventChangeDTO' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEventChangeDTO' event)
      "SetLevelEvent" -> parseJSON (Object o) >>= \event -> return (SetLevelEventChangeDTO' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEventChangeDTO' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON SetReplyEventChangeDTO where
  parseJSON = simpleParseJSON "_setReplyEventChangeDTO"

instance ToJSON SetReplyEventChangeDTO where
  toJSON = simpleToJSON''' "_setReplyEventChangeDTO" "type" "ChangeDTO"

instance FromJSON ClearReplyEventChangeDTO where
  parseJSON = simpleParseJSON "_clearReplyEventChangeDTO"

instance ToJSON ClearReplyEventChangeDTO where
  toJSON = simpleToJSON''' "_clearReplyEventChangeDTO" "type" "ChangeDTO"

instance FromJSON SetLevelEventChangeDTO where
  parseJSON = simpleParseJSON "_setLevelEventChangeDTO"

instance ToJSON SetLevelEventChangeDTO where
  toJSON = simpleToJSON''' "_setLevelEventChangeDTO" "type" "ChangeDTO"

instance FromJSON SetLabelsEventChangeDTO where
  parseJSON = simpleParseJSON "_setLabelsEventChangeDTO"

instance ToJSON SetLabelsEventChangeDTO where
  toJSON = simpleToJSON''' "_setLabelsEventChangeDTO" "type" "ChangeDTO"
