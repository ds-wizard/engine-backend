module Wizard.Api.Resource.User.UserProfileChangeJM where

import Data.Aeson

import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserProfileChangeDTO where
  parseJSON = simpleParseJSON "_userProfileChangeDTO"

instance ToJSON UserProfileChangeDTO where
  toJSON = simpleToJSON "_userProfileChangeDTO"
