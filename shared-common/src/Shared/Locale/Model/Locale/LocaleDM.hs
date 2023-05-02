module Shared.Locale.Model.Locale.LocaleDM where

import Shared.Common.Constant.App
import Shared.Common.Util.Date
import Shared.Locale.Constant.Locale
import Shared.Locale.Model.Locale.Locale

localeDefault :: Locale
localeDefault =
  Locale
    { lId = defaultLocaleId
    , name = "English"
    , description = "Default English locale"
    , code = "en"
    , organizationId = "wizard"
    , localeId = "default"
    , version = "1.0.0"
    , defaultLocale = True
    , license = "Apache-2.0"
    , readme = "# English Locale"
    , recommendedAppVersion = "3.18.0"
    , enabled = True
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    , updatedAt = dt' 2022 1 21
    }
