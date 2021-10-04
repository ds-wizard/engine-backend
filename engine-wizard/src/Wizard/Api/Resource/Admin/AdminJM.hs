module Wizard.Api.Resource.Admin.AdminJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Admin.Admin

instance FromJSON AdminSection where
  parseJSON = simpleParseJSON "_adminSection"

instance ToJSON AdminSection where
  toJSON = simpleToJSON "_adminSection"

instance FromJSON AdminOperation where
  parseJSON = simpleParseJSON "_adminOperation"

instance ToJSON AdminOperation where
  toJSON = simpleToJSON "_adminOperation"

instance FromJSON AdminOperationParameter where
  parseJSON = simpleParseJSON "_adminOperationParameter"

instance ToJSON AdminOperationParameter where
  toJSON = simpleToJSON "_adminOperationParameter"

instance FromJSON AdminOperationParameterType

instance ToJSON AdminOperationParameterType
