module Wizard.Api.Resource.PersistentCommand.PersistentCommandJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.PersistentCommand.PersistentCommand

instance FromJSON PersistentCommandState

instance ToJSON PersistentCommandState

instance FromJSON PersistentCommandDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandDTO where
  toJSON = genericToJSON jsonOptions
