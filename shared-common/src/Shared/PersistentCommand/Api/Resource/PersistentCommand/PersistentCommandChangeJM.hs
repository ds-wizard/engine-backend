module Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM ()

instance FromJSON PersistentCommandChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandChangeDTO where
  toJSON = genericToJSON jsonOptions
