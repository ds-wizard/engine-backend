module Registry.Api.Resource.Locale.LocaleDetailSM where

import Data.Swagger

import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Api.Resource.Locale.LocaleDetailJM ()
import Registry.Api.Resource.Package.PackageSimpleSM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Locale.LocaleMapper
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO localeNl [] orgGlobal)
