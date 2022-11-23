module Registry.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Locale.LocaleJM ()
import Registry.Service.Locale.LocaleMapper
import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger (toDTO [] localeNl)
