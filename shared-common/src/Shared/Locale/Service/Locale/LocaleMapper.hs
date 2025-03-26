module Shared.Locale.Service.Locale.LocaleMapper where

import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSuggestion

toLocaleSuggestion :: Locale -> LocaleSuggestion
toLocaleSuggestion locale =
  LocaleSuggestion
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , defaultLocale = locale.defaultLocale
    }
