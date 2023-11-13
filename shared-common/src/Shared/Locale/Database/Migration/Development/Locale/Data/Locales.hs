module Shared.Locale.Database.Migration.Development.Locale.Data.Locales where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Locale.Constant.Locale
import Shared.Locale.Model.Locale.Locale

localeDefaultEn :: Locale
localeDefaultEn =
  Locale
    { lId = defaultLocaleId
    , name = "English"
    , description = "English Locale"
    , code = "en"
    , organizationId = "wizard"
    , localeId = "default"
    , version = "1.0.0"
    , defaultLocale = True
    , license = "Apache-2.0"
    , readme = "# English Locale"
    , recommendedAppVersion = "3.15.0"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    , updatedAt = dt' 2022 1 21
    }

localeNl :: Locale
localeNl =
  Locale
    { lId = "global:dutch:1.0.0"
    , name = "Dutch"
    , description = "Dutch translation"
    , code = "nl"
    , organizationId = "global"
    , localeId = "dutch"
    , version = "1.0.0"
    , defaultLocale = False
    , license = "Apache-2.0"
    , readme = "# Dutch Translation"
    , recommendedAppVersion = "3.15.0"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    , updatedAt = dt' 2022 1 21
    }

localeNlContent :: BS.ByteString
localeNlContent = BSL.toStrict . encode . M.insert "someKey" "someValue" $ M.empty

localeNlEdited :: Locale
localeNlEdited = localeNl {enabled = False, defaultLocale = False}

localeDe :: Locale
localeDe =
  Locale
    { lId = "global:german:1.0.0"
    , name = "German"
    , description = "German translation"
    , code = "de"
    , organizationId = "global"
    , localeId = "german"
    , version = "1.0.0"
    , defaultLocale = False
    , license = "Apache-2.0"
    , readme = "# German Translation"
    , recommendedAppVersion = "3.15.0"
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    , updatedAt = dt' 2022 1 21
    }

localeDeContent :: BS.ByteString
localeDeContent = BSL.toStrict . encode . M.insert "someKey" "someValue" $ M.empty

differentLocale :: Locale
differentLocale =
  Locale
    { lId = "different:translation:1.0.0"
    , name = "Different"
    , description = "Different translation"
    , code = "diff"
    , organizationId = "different"
    , localeId = "translation"
    , version = "1.0.0"
    , defaultLocale = True
    , license = "Apache-2.0"
    , readme = "# Different Translation"
    , recommendedAppVersion = "3.15.0"
    , enabled = True
    , tenantUuid = differentTenantUuid
    , createdAt = dt' 2022 1 21
    , updatedAt = dt' 2022 1 21
    }

differentLocaleContent :: BS.ByteString
differentLocaleContent = BSL.toStrict . encode . M.insert "differentKey" "differentValue" $ M.empty
