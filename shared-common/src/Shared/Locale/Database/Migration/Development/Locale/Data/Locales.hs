module Shared.Locale.Database.Migration.Development.Locale.Data.Locales where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.Locale.Constant.Locale
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSimple
import Shared.Locale.Model.Locale.LocaleSuggestion
import Shared.Locale.Service.Locale.LocaleMapper

localeDefaultEn :: Locale
localeDefaultEn =
  Locale
    { uuid = u' "7fb838c5-9279-4a78-8c2b-86ee9762a95f"
    , name = "English"
    , description = "English Locale"
    , code = "en"
    , organizationId = defaultLocaleOrganizationId
    , localeId = defaultLocaleLocaleId
    , version = defaultLocaleVersion
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
    { uuid = u' "d9894fb9-c6a5-4294-98d6-b46d75684d53"
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

localeNlSuggestion :: LocaleSuggestion
localeNlSuggestion = toLocaleSuggestion localeNl

localeNlEdited :: Locale
localeNlEdited = localeNl {enabled = False, defaultLocale = False}

localeNlSimple :: LocaleSimple
localeNlSimple =
  LocaleSimple
    { uuid = localeNl.uuid
    , name = localeNl.name
    , code = localeNl.code
    , defaultLocale = localeNl.defaultLocale
    }

localeDe :: Locale
localeDe =
  Locale
    { uuid = u' "22fed2bf-9f00-4cf6-a4d4-4a845a0bc273"
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
    { uuid = u' "6881ecac-9e88-4ddd-9df9-d6df166b1c9b"
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
