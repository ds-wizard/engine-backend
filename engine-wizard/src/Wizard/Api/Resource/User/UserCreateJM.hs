module Wizard.Api.Resource.User.UserCreateJM where

import Data.Aeson

import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserCreateDTO where
  parseJSON = simpleParseJSON "_userCreateDTO"

instance ToJSON UserCreateDTO where
  toJSON = simpleToJSON "_userCreateDTO"
