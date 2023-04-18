module Wizard.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Api.Resource.Locale.LocaleStateSM ()
import Wizard.Database.Migration.Development.Locale.Data.Locales
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger localeNlDto
