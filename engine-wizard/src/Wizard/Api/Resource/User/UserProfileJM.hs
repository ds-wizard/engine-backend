module Wizard.Api.Resource.User.UserProfileJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()

instance FromJSON UserProfileDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserProfileDTO where
  toJSON = genericToJSON simpleOptions
