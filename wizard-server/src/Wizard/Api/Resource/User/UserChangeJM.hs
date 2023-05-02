module Wizard.Api.Resource.User.UserChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserChangeDTO

instance FromJSON UserChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserChangeDTO where
  toJSON = genericToJSON jsonOptions
