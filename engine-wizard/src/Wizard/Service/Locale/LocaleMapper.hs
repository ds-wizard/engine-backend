module Wizard.Service.Locale.LocaleMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Model.Locale.Locale

toDTO :: Locale -> LocaleDTO
toDTO locale =
  LocaleDTO {_localeDTOName = locale ^. name, _localeDTOCode = locale ^. code, _localeDTOFallback = locale ^. fallback}
