module Shared.Model.Locale.LocaleDM where

import Shared.Constant.App
import Shared.Constant.Locale
import Shared.Model.Locale.Locale
import Shared.Util.Date

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
