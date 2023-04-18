module Wizard.Api.Resource.User.UserProfileChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()

instance FromJSON UserProfileChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserProfileChangeDTO where
  toJSON = genericToJSON jsonOptions
