module Wizard.Api.Resource.User.UserChangeJM where

import Data.Aeson

import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserChangeDTO where
  parseJSON = simpleParseJSON "_userChangeDTO"

instance ToJSON UserChangeDTO where
  toJSON = simpleToJSON "_userChangeDTO"
