module Wizard.Service.Locale.LocaleMapper where

import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Model.Locale.Locale

toDTO :: Locale -> LocaleDTO
toDTO locale =
  LocaleDTO {name = locale.name, code = locale.code, fallback = locale.fallback}
