module Wizard.Api.Resource.User.UserChangeJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserChangeDTO

instance FromJSON UserChangeDTO where
  parseJSON = simpleParseJSON "_userChangeDTO"

instance ToJSON UserChangeDTO where
  toJSON = simpleToJSON "_userChangeDTO"
