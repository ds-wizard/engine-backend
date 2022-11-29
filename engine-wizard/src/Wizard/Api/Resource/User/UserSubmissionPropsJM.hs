module Wizard.Api.Resource.User.UserSubmissionPropsJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.User.User

instance FromJSON UserSubmissionProps where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionProps where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserSubmissionPropsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionPropsDTO where
  toJSON = genericToJSON jsonOptions
