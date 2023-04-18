module Wizard.Api.Resource.Registry.RegistryOrganizationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Registry.RegistryOrganization

instance FromJSON RegistryOrganization where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryOrganization where
  toJSON = genericToJSON jsonOptions
