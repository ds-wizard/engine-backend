module Registry.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Locale.LocaleJM ()
import Registry.Service.Locale.LocaleMapper
import Shared.Common.Util.Swagger
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger (toDTO [] localeNl)
