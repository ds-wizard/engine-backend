module Wizard.Api.Resource.Dev.DevExecutionResultJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Dev.DevExecutionResultDTO

instance FromJSON AdminExecutionResultDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AdminExecutionResultDTO where
  toJSON = genericToJSON simpleOptions
