module Wizard.Api.Resource.Dev.DevExecutionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Dev.DevExecutionDTO

instance FromJSON DevExecutionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DevExecutionDTO where
  toJSON = genericToJSON simpleOptions
