module Wizard.Api.Resource.User.UserSubmissionPropListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.User.UserSubmissionPropList

instance FromJSON UserSubmissionPropList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionPropList where
  toJSON = genericToJSON jsonOptions
