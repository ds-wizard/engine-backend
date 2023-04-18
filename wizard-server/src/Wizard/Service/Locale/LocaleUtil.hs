module Wizard.Service.Locale.LocaleUtil where

import qualified Data.List as L

import Shared.Locale.Model.Locale.Locale
import Wizard.Model.Locale.LocaleList
import Wizard.Model.Locale.LocaleState
import Wizard.Model.Registry.RegistryLocale
import WizardLib.Common.Util.Coordinate

computeLocaleState :: [RegistryLocale] -> Locale -> LocaleState
computeLocaleState localesFromRegistry locale =
  case selectLocaleByOrgIdAndLocaleId locale localesFromRegistry of
    Just localeFromRegistry ->
      case compareVersion localeFromRegistry.remoteVersion locale.version of
        LT -> UnpublishedLocaleState
        EQ -> UpToDateLocaleState
        GT -> OutdatedLocaleState
    Nothing -> UnknownLocaleState

computeLocaleState' :: Bool -> LocaleList -> LocaleState
computeLocaleState' registryEnabled locale
  | registryEnabled = locale.state
  | otherwise = UnknownLocaleState

selectLocaleByOrgIdAndLocaleId locale =
  L.find (\l -> l.organizationId == locale.organizationId && l.localeId == locale.localeId)

selectOrganizationByOrgId locale = L.find (\org -> org.organizationId == locale.organizationId)
