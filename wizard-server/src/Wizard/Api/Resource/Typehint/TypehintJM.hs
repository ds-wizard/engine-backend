module Wizard.Api.Resource.Typehint.TypehintJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Typehint.TypehintDTO

instance FromJSON TypehintDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypehintDTO where
  toJSON = genericToJSON jsonOptions
