module Wizard.Api.Resource.Registry.RegistryConfirmationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO

instance FromJSON RegistryConfirmationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryConfirmationDTO where
  toJSON = genericToJSON jsonOptions
