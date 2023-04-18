module Registry.Api.Resource.Locale.LocaleDetailSM where

import Data.Swagger

import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Api.Resource.Locale.LocaleDetailJM ()
import Registry.Api.Resource.Package.PackageSimpleSM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Locale.LocaleMapper
import Shared.Common.Util.Swagger
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO localeNl [] orgGlobal)
