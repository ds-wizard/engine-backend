module Wizard.Integration.Resource.Config.CompileClientCssIJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Integration.Resource.Config.CompileClientCssIDTO

instance FromJSON CompileClientCssIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CompileClientCssIDTO where
  toJSON = genericToJSON jsonOptions
