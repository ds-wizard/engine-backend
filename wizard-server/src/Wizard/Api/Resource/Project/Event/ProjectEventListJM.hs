module Wizard.Api.Resource.Project.Event.ProjectEventListJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Model.Project.Event.ProjectEventList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance ToJSON ProjectEventList where
  toJSON = toSumJSONWithTypeField "type" "List"

instance FromJSON ProjectEventList where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEventList' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEventList' event)
      "SetPhaseEvent" -> parseJSON (Object o) >>= \event -> return (SetPhaseEventList' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEventList' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON SetReplyEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetReplyEventList where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClearReplyEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClearReplyEventList where
  toJSON = genericToJSON jsonOptions

instance FromJSON SetPhaseEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetPhaseEventList where
  toJSON = genericToJSON jsonOptions

instance FromJSON SetLabelsEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetLabelsEventList where
  toJSON = genericToJSON jsonOptions
