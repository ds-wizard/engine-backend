module Wizard.Api.Resource.PersistentCommand.PersistentCommandJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance FromJSON PersistentCommandDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandDTO where
  toJSON = genericToJSON jsonOptions
