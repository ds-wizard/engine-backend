module RegistryLib.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import RegistryLib.Api.Resource.Locale.LocaleDTO
import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
