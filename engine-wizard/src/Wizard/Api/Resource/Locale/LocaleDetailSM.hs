module Wizard.Api.Resource.Locale.LocaleDetailSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Api.Resource.Locale.LocaleDetailJM ()
import Wizard.Api.Resource.Locale.LocaleStateSM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDetailDTO where
  declareNamedSchema = toSwagger localeNlDetailDto
