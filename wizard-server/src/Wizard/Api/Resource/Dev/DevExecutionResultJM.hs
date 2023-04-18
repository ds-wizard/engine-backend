module Wizard.Api.Resource.Dev.DevExecutionResultJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Dev.DevExecutionResultDTO

instance FromJSON AdminExecutionResultDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AdminExecutionResultDTO where
  toJSON = genericToJSON jsonOptions
