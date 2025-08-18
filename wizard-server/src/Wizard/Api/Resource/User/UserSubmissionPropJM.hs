module Wizard.Api.Resource.User.UserSubmissionPropJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.User.UserSubmissionProp

instance FromJSON UserSubmissionProp where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionProp where
  toJSON = genericToJSON jsonOptions
