module Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandJM ()

instance FromJSON PersistentCommandChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandChangeDTO where
  toJSON = genericToJSON jsonOptions
