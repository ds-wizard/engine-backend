module Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance FromJSON PersistentCommandDetailDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PersistentCommandDetailDTO where
  toJSON = genericToJSON simpleOptions
