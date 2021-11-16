module Wizard.Integration.Resource.Config.CompileClientCssIJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Integration.Resource.Config.CompileClientCssIDTO

instance FromJSON CompileClientCssIDTO where
  parseJSON = simpleParseJSON "_compileClientCssIDTO"

instance ToJSON CompileClientCssIDTO where
  toJSON = simpleToJSON "_compileClientCssIDTO"
