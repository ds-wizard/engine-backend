module Registry.Api.Resource.Locale.LocaleDetailSM where

import Data.Swagger

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Api.Resource.Locale.LocaleDetailJM ()
import Registry.Service.Locale.LocaleMapper
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO localeNl [] orgGlobal)
