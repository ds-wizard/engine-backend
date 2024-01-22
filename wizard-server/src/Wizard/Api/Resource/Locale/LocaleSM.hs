module Wizard.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger localeNlDto
