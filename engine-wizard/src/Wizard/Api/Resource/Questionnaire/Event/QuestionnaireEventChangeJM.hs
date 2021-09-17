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
      "SetPhaseEvent" -> parseJSON (Object o) >>= \event -> return (SetPhaseEventChangeDTO' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEventChangeDTO' event)
      "ResolveCommentThreadEvent" ->
        parseJSON (Object o) >>= \event -> return (ResolveCommentThreadEventChangeDTO' event)
      "ReopenCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (ReopenCommentThreadEventChangeDTO' event)
      "DeleteCommentThreadEvent" -> parseJSON (Object o) >>= \event -> return (DeleteCommentThreadEventChangeDTO' event)
      "AddCommentEvent" -> parseJSON (Object o) >>= \event -> return (AddCommentEventChangeDTO' event)
      "EditCommentEvent" -> parseJSON (Object o) >>= \event -> return (EditCommentEventChangeDTO' event)
      "DeleteCommentEvent" -> parseJSON (Object o) >>= \event -> return (DeleteCommentEventChangeDTO' event)
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

instance FromJSON SetPhaseEventChangeDTO where
  parseJSON = simpleParseJSON "_setPhaseEventChangeDTO"

instance ToJSON SetPhaseEventChangeDTO where
  toJSON = simpleToJSON''' "_setPhaseEventChangeDTO" "type" "ChangeDTO"

instance FromJSON SetLabelsEventChangeDTO where
  parseJSON = simpleParseJSON "_setLabelsEventChangeDTO"

instance ToJSON SetLabelsEventChangeDTO where
  toJSON = simpleToJSON''' "_setLabelsEventChangeDTO" "type" "ChangeDTO"

instance FromJSON ResolveCommentThreadEventChangeDTO where
  parseJSON = simpleParseJSON "_resolveCommentThreadEventChangeDTO"

instance ToJSON ResolveCommentThreadEventChangeDTO where
  toJSON = simpleToJSON''' "_resolveCommentThreadEventChangeDTO" "type" "ChangeDTO"

instance FromJSON ReopenCommentThreadEventChangeDTO where
  parseJSON = simpleParseJSON "_reopenCommentThreadEventChangeDTO"

instance ToJSON ReopenCommentThreadEventChangeDTO where
  toJSON = simpleToJSON''' "_reopenCommentThreadEventChangeDTO" "type" "ChangeDTO"

instance FromJSON DeleteCommentThreadEventChangeDTO where
  parseJSON = simpleParseJSON "_deleteCommentThreadEventChangeDTO"

instance ToJSON DeleteCommentThreadEventChangeDTO where
  toJSON = simpleToJSON''' "_deleteCommentThreadEventChangeDTO" "type" "ChangeDTO"

instance FromJSON AddCommentEventChangeDTO where
  parseJSON = simpleParseJSON "_addCommentEventChangeDTO"

instance ToJSON AddCommentEventChangeDTO where
  toJSON = simpleToJSON''' "_addCommentEventChangeDTO" "type" "ChangeDTO"

instance FromJSON EditCommentEventChangeDTO where
  parseJSON = simpleParseJSON "_editCommentEventChangeDTO"

instance ToJSON EditCommentEventChangeDTO where
  toJSON = simpleToJSON''' "_editCommentEventChangeDTO" "type" "ChangeDTO"

instance FromJSON DeleteCommentEventChangeDTO where
  parseJSON = simpleParseJSON "_deleteCommentEventChangeDTO"

instance ToJSON DeleteCommentEventChangeDTO where
  toJSON = simpleToJSON''' "_deleteCommentEventChangeDTO" "type" "ChangeDTO"
