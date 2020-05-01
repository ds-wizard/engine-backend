module Wizard.Api.Resource.User.UserProfileChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()

instance FromJSON UserProfileChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserProfileChangeDTO where
  toJSON = genericToJSON simpleOptions
