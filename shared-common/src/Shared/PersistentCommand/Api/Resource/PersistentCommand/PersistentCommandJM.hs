module Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance FromJSON PersistentCommandState

instance ToJSON PersistentCommandState

instance FromJSON identity => FromJSON (PersistentCommand identity) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON identity => ToJSON (PersistentCommand identity) where
  toJSON = genericToJSON jsonOptions
