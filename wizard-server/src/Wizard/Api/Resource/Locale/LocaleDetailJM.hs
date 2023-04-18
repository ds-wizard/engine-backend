module Wizard.Api.Resource.Locale.LocaleDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Api.Resource.Locale.LocaleStateJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()

instance FromJSON LocaleDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDetailDTO where
  toJSON = genericToJSON jsonOptions
