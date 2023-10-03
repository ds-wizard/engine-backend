module Registry.Api.Resource.PersistentCommand.PersistentCommandJM where

import Data.Aeson

import Registry.Api.Resource.PersistentCommand.PersistentCommandDTO
import Shared.Common.Util.Aeson
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM ()

instance FromJSON PersistentCommandDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandDTO where
  toJSON = genericToJSON jsonOptions
