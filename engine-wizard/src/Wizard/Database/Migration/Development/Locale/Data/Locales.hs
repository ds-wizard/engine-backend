module Wizard.Database.Migration.Development.Locale.Data.Locales where

import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Model.Locale.Locale
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Model.Locale.LocaleList
import Wizard.Model.Locale.LocaleState
import Wizard.Service.Locale.LocaleMapper

localeListDefaultEn :: LocaleList
localeListDefaultEn = toLocaleList localeDefaultEn UnknownLocaleState

localeDefaultEnDto :: LocaleDTO
localeDefaultEnDto = toDTO False localeListDefaultEn

localeListNl :: LocaleList
localeListNl = toLocaleList localeNl UnknownLocaleState

localeNlDto :: LocaleDTO
localeNlDto = (toDTO False localeListNl) {organization = Just orgGlobalSimple}

localeNlDetailDto :: LocaleDetailDTO
localeNlDetailDto = toDetailDTO localeNl [] [globalRegistryOrganization] [localeNl.version] Nothing

localeNlChangeDto :: LocaleChangeDTO
localeNlChangeDto =
  LocaleChangeDTO
    { enabled = False
    , defaultLocale = False
    }

localeListDe :: LocaleList
localeListDe = toLocaleList localeDe UnknownLocaleState

localeDeDto :: LocaleDTO
localeDeDto = (toDTO False localeListDe) {organization = Just orgGlobalSimple}
