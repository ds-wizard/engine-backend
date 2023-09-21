module Registry.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Registry.Service.Locale.LocaleMapper
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Locale.LocaleJM ()
import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Common.Util.Swagger
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger (toDTO [] localeNl)
