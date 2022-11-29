module Registry.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import Registry.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
