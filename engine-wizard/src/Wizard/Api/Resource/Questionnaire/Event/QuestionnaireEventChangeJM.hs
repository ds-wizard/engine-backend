module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM where

import Control.Monad
import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance ToJSON QuestionnaireEventChangeDTO where
  toJSON = toSumJSONWithTypeField "type" "ChangeDTO"

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetReplyEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClearReplyEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClearReplyEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON SetPhaseEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetPhaseEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON SetLabelsEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetLabelsEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ResolveCommentThreadEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ResolveCommentThreadEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ReopenCommentThreadEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ReopenCommentThreadEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DeleteCommentThreadEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteCommentThreadEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON AddCommentEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddCommentEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON EditCommentEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditCommentEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DeleteCommentEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteCommentEventChangeDTO where
  toJSON = genericToJSON jsonOptions
