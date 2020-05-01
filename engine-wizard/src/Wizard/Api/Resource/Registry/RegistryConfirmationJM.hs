module Wizard.Api.Resource.Registry.RegistryConfirmationJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO

instance FromJSON RegistryConfirmationDTO where
  parseJSON = simpleParseJSON "_registryConfirmationDTO"

instance ToJSON RegistryConfirmationDTO where
  toJSON = simpleToJSON "_registryConfirmationDTO"
