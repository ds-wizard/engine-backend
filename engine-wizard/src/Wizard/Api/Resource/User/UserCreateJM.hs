module Wizard.Api.Resource.User.UserCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserCreateDTO

instance FromJSON UserCreateDTO where
  parseJSON = simpleParseJSON "_userCreateDTO"

instance ToJSON UserCreateDTO where
  toJSON = simpleToJSON "_userCreateDTO"
