module Wizard.Api.Resource.User.UserProfileJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()

instance FromJSON UserProfileDTO where
  parseJSON = simpleParseJSON "_userProfileDTO"

instance ToJSON UserProfileDTO where
  toJSON = simpleToJSON "_userProfileDTO"
