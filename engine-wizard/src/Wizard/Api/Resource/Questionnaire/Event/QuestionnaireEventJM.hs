module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM where

import Control.Monad
import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireEvent

instance ToJSON QuestionnaireEventDTO where
  toJSON = toSumJSON

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetReplyEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ClearReplyEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClearReplyEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON SetPhaseEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetPhaseEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON SetLabelsEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetLabelsEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ResolveCommentThreadEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ResolveCommentThreadEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ReopenCommentThreadEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ReopenCommentThreadEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON DeleteCommentThreadEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteCommentThreadEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON AddCommentEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddCommentEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON EditCommentEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditCommentEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON DeleteCommentEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteCommentEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON QuestionnaireEvent where
  toJSON = toSumJSONWithTypeField "type" ""

instance FromJSON QuestionnaireEvent where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEvent' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEvent' event)
      "SetPhaseEvent" -> parseJSON (Object o) >>= \event -> return (SetPhaseEvent' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEvent' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON SetReplyEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetReplyEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ClearReplyEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClearReplyEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON SetPhaseEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetPhaseEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON SetLabelsEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetLabelsEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
