module Wizard.Api.Resource.Admin.AdminExecutionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Admin.AdminExecutionDTO

instance FromJSON AdminExecutionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AdminExecutionDTO where
  toJSON = genericToJSON simpleOptions
