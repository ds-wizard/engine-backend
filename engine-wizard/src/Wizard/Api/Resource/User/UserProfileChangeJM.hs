module Wizard.Api.Resource.User.UserProfileChangeJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()

instance FromJSON UserProfileChangeDTO where
  parseJSON = simpleParseJSON "_userProfileChangeDTO"

instance ToJSON UserProfileChangeDTO where
  toJSON = simpleToJSON "_userProfileChangeDTO"
