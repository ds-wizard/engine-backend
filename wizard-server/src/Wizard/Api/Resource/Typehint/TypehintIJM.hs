module Wizard.Api.Resource.Typehint.TypehintIJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Integration.Resource.Typehint.TypehintIDTO

instance FromJSON TypehintIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypehintIDTO where
  toJSON = genericToJSON jsonOptions
