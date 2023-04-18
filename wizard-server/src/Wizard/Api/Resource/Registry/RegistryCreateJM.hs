module Wizard.Api.Resource.Registry.RegistryCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Registry.RegistryCreateDTO

instance FromJSON RegistryCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryCreateDTO where
  toJSON = genericToJSON jsonOptions
