module Wizard.Api.Resource.User.UserStateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserStateDTO

instance FromJSON UserStateDTO where
  parseJSON = simpleParseJSON "_userStateDTO"

instance ToJSON UserStateDTO where
  toJSON = simpleToJSON "_userStateDTO"
