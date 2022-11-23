module Registry.Api.Resource.Locale.LocaleDetailJM where

import Data.Aeson

import Registry.Api.Resource.Locale.LocaleDetailDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson

instance FromJSON LocaleDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDetailDTO where
  toJSON = genericToJSON jsonOptions
