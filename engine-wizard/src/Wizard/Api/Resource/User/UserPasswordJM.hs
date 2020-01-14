module Wizard.Api.Resource.User.UserPasswordJM where

import Data.Aeson

import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserPasswordDTO where
  parseJSON = simpleParseJSON "_userPasswordDTO"

instance ToJSON UserPasswordDTO where
  toJSON = simpleToJSON "_userPasswordDTO"
