module Wizard.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleStateJM ()
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
