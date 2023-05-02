module Wizard.Api.Resource.User.UserProfileJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()

instance FromJSON UserProfileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserProfileDTO where
  toJSON = genericToJSON jsonOptions
