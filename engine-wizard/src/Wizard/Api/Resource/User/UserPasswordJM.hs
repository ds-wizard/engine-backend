module Wizard.Api.Resource.User.UserPasswordJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserPasswordDTO

instance FromJSON UserPasswordDTO where
  parseJSON = simpleParseJSON "_userPasswordDTO"

instance ToJSON UserPasswordDTO where
  toJSON = simpleToJSON "_userPasswordDTO"
