module RegistryLib.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Common.Util.Aeson

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
