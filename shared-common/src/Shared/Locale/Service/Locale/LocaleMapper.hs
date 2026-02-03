module Shared.Locale.Service.Locale.LocaleMapper where

import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSimple
import Shared.Locale.Model.Locale.LocaleSuggestion

toLocaleSuggestion :: Locale -> LocaleSuggestion
toLocaleSuggestion locale =
  LocaleSuggestion
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    }

toSimple :: Locale -> LocaleSimple
toSimple Locale {..} = LocaleSimple {..}
