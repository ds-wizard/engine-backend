module Shared.Locale.Api.Resource.Locale.LocaleSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Locale.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.LocaleSimple

instance ToSchema LocaleSimple where
  declareNamedSchema = toSwagger localeNlSimple
