module Wizard.Api.Resource.Dev.DevExecutionJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Dev.DevExecutionDTO

instance FromJSON DevExecutionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevExecutionDTO where
  toJSON = genericToJSON jsonOptions
