module Wizard.Api.Resource.Registry.RegistryCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Registry.RegistryCreateDTO

instance FromJSON RegistryCreateDTO where
  parseJSON = simpleParseJSON "_registryCreateDTO"

instance ToJSON RegistryCreateDTO where
  toJSON = simpleToJSON "_registryCreateDTO"
