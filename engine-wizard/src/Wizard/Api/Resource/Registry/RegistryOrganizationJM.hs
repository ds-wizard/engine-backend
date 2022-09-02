module Wizard.Api.Resource.Registry.RegistryOrganizationJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Registry.RegistryOrganization

instance FromJSON RegistryOrganization where
  parseJSON = simpleParseJSON "_registryOrganization"

instance ToJSON RegistryOrganization where
  toJSON = simpleToJSON "_registryOrganization"
