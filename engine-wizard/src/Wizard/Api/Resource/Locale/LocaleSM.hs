module Wizard.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Api.Resource.Locale.LocaleStateSM ()
import Wizard.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger localeNlDto
