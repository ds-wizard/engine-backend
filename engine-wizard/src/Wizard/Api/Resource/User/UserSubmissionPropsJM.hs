module Wizard.Api.Resource.User.UserSubmissionPropsJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.User.User

instance FromJSON UserSubmissionProps where
  parseJSON = simpleParseJSON "_userSubmissionProps"

instance ToJSON UserSubmissionProps where
  toJSON = simpleToJSON "_userSubmissionProps"

instance FromJSON UserSubmissionPropsDTO where
  parseJSON = simpleParseJSON "_userSubmissionPropsDTO"

instance ToJSON UserSubmissionPropsDTO where
  toJSON = simpleToJSON "_userSubmissionPropsDTO"
