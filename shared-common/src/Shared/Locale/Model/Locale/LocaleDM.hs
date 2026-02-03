module Shared.Locale.Model.Locale.LocaleDM where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.Locale.Constant.Locale
import Shared.Locale.Model.Locale.Locale

localeDefault :: Locale
localeDefault =
  Locale
    { uuid = u' "7fb838c5-9279-4a78-8c2b-86ee9762a95f"
    , name = "English"
    , description = "Default English locale"
    , code = "en"
    , organizationId = defaultLocaleOrganizationId
    , localeId = defaultLocaleLocaleId
    , version = defaultLocaleVersion
    , defaultLocale = True
    , license = "Apache-2.0"
    , readme = "# English Locale"
    , recommendedAppVersion = "3.18.0"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    , updatedAt = dt' 2022 1 21
    }
