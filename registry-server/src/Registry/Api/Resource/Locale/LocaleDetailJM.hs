module Registry.Api.Resource.Locale.LocaleDetailJM where

import Data.Aeson

import Registry.Api.Resource.Locale.LocaleDetailDTO
import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

instance FromJSON LocaleDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDetailDTO where
  toJSON = genericToJSON jsonOptions
