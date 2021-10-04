module Wizard.Api.Resource.Admin.AdminExecutionResultJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Admin.AdminExecutionResultDTO

instance FromJSON AdminExecutionResultDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AdminExecutionResultDTO where
  toJSON = genericToJSON simpleOptions
