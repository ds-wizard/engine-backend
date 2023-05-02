module Wizard.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Model.ActionKey.ActionKey

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType

instance FromJSON ActionKeyDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ActionKeyDTO where
  toJSON = genericToJSON jsonOptions
