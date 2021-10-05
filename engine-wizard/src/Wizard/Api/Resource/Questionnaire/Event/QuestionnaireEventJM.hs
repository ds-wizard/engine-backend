module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireEvent

instance ToJSON QuestionnaireEventDTO where
  toJSON = toSumJSON' "type"

instance FromJSON QuestionnaireEventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEventDTO' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEventDTO' event)
      "SetPhaseEvent" -> parseJSON (Object o) >>= \event -> return (SetPhaseEventDTO' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEventDTO' event)
      "ResolveCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (ResolveCommentThreadEventDTO' event)
      "ReopenCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (ReopenCommentThreadEventDTO' event)
      "DeleteCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (DeleteCommentThreadEventDTO' event)
      "AddCommentEvent" -> parseJSON (Object o) >>= \event -> return (AddCommentEventDTO' event)
      "EditCommentEvent" -> parseJSON (Object o) >>= \event -> return (EditCommentEventDTO' event)
      "DeleteCommentEvent" -> parseJSON (Object o) >>= \event -> return (DeleteCommentEventDTO' event)
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

instance FromJSON SetPhaseEventDTO where
  parseJSON = simpleParseJSON "_setPhaseEventDTO"

instance ToJSON SetPhaseEventDTO where
  toJSON = simpleToJSON' "_setPhaseEventDTO" "type"

instance FromJSON SetLabelsEventDTO where
  parseJSON = simpleParseJSON "_setLabelsEventDTO"

instance ToJSON SetLabelsEventDTO where
  toJSON = simpleToJSON' "_setLabelsEventDTO" "type"

instance FromJSON ResolveCommentThreadEventDTO where
  parseJSON = simpleParseJSON "_resolveCommentThreadEventDTO"

instance ToJSON ResolveCommentThreadEventDTO where
  toJSON = simpleToJSON' "_resolveCommentThreadEventDTO" "type"

instance FromJSON ReopenCommentThreadEventDTO where
  parseJSON = simpleParseJSON "_reopenCommentThreadEventDTO"

instance ToJSON ReopenCommentThreadEventDTO where
  toJSON = simpleToJSON' "_reopenCommentThreadEventDTO" "type"

instance FromJSON DeleteCommentThreadEventDTO where
  parseJSON = simpleParseJSON "_deleteCommentThreadEventDTO"

instance ToJSON DeleteCommentThreadEventDTO where
  toJSON = simpleToJSON' "_deleteCommentThreadEventDTO" "type"

instance FromJSON AddCommentEventDTO where
  parseJSON = simpleParseJSON "_addCommentEventDTO"

instance ToJSON AddCommentEventDTO where
  toJSON = simpleToJSON' "_addCommentEventDTO" "type"

instance FromJSON EditCommentEventDTO where
  parseJSON = simpleParseJSON "_editCommentEventDTO"

instance ToJSON EditCommentEventDTO where
  toJSON = simpleToJSON' "_editCommentEventDTO" "type"

instance FromJSON DeleteCommentEventDTO where
  parseJSON = simpleParseJSON "_deleteCommentEventDTO"

instance ToJSON DeleteCommentEventDTO where
  toJSON = simpleToJSON' "_deleteCommentEventDTO" "type"

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON QuestionnaireEvent where
  toJSON = toSumJSON' "type"

instance FromJSON QuestionnaireEvent where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEvent' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEvent' event)
      "SetPhaseEvent" -> parseJSON (Object o) >>= \event -> return (SetPhaseEvent' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEvent' event)
      "ResolveCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (ResolveCommentThreadEvent' event)
      "ReopenCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (ReopenCommentThreadEvent' event)
      "DeleteCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (DeleteCommentThreadEvent' event)
      "AddCommentEvent" -> parseJSON (Object o) >>= \event -> return (AddCommentEvent' event)
      "EditCommentEvent" -> parseJSON (Object o) >>= \event -> return (EditCommentEvent' event)
      "DeleteCommentEvent" -> parseJSON (Object o) >>= \event -> return (DeleteCommentEvent' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON SetReplyEvent where
  parseJSON = simpleParseJSON "_setReplyEvent"

instance ToJSON SetReplyEvent where
  toJSON = simpleToJSON' "_setReplyEvent" "type"

instance FromJSON ClearReplyEvent where
  parseJSON = simpleParseJSON "_clearReplyEvent"

instance ToJSON ClearReplyEvent where
  toJSON = simpleToJSON' "_clearReplyEvent" "type"

instance FromJSON SetPhaseEvent where
  parseJSON = simpleParseJSON "_setPhaseEvent"

instance ToJSON SetPhaseEvent where
  toJSON = simpleToJSON' "_setPhaseEvent" "type"

instance FromJSON SetLabelsEvent where
  parseJSON = simpleParseJSON "_setLabelsEvent"

instance ToJSON SetLabelsEvent where
  toJSON = simpleToJSON' "_setLabelsEvent" "type"

instance FromJSON ResolveCommentThreadEvent where
  parseJSON = simpleParseJSON "_resolveCommentThreadEvent"

instance ToJSON ResolveCommentThreadEvent where
  toJSON = simpleToJSON' "_resolveCommentThreadEvent" "type"

instance FromJSON ReopenCommentThreadEvent where
  parseJSON = simpleParseJSON "_reopenCommentThreadEvent"

instance ToJSON ReopenCommentThreadEvent where
  toJSON = simpleToJSON' "_reopenCommentThreadEvent" "type"

instance FromJSON DeleteCommentThreadEvent where
  parseJSON = simpleParseJSON "_deleteCommentThreadEvent"

instance ToJSON DeleteCommentThreadEvent where
  toJSON = simpleToJSON' "_deleteCommentThreadEvent" "type"

instance FromJSON AddCommentEvent where
  parseJSON = simpleParseJSON "_addCommentEvent"

instance ToJSON AddCommentEvent where
  toJSON = simpleToJSON' "_addCommentEvent" "type"

instance FromJSON EditCommentEvent where
  parseJSON = simpleParseJSON "_editCommentEvent"

instance ToJSON EditCommentEvent where
  toJSON = simpleToJSON' "_editCommentEvent" "type"

instance FromJSON DeleteCommentEvent where
  parseJSON = simpleParseJSON "_deleteCommentEvent"

instance ToJSON DeleteCommentEvent where
  toJSON = simpleToJSON' "_deleteCommentEvent" "type"
