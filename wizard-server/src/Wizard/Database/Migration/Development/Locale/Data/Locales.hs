module Wizard.Database.Migration.Development.Locale.Data.Locales where

import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Model.Locale.LocaleList
import Wizard.Service.Locale.LocaleMapper

localeListDefaultEn :: LocaleList
localeListDefaultEn = toLocaleList localeDefaultEn

localeDefaultEnDto :: LocaleDTO
localeDefaultEnDto = toDTO False localeListDefaultEn

localeListNl :: LocaleList
localeListNl = toLocaleList localeNl

localeNlDto :: LocaleDTO
localeNlDto = (toDTO False localeListNl) {organization = Just orgGlobalSimple}

localeNlDetailDto :: LocaleDetailDTO
localeNlDetailDto = toDetailDTO localeNl True [] [globalRegistryOrganization] [localeNl.version] Nothing

localeNlChangeDto :: LocaleChangeDTO
localeNlChangeDto =
  LocaleChangeDTO
    { enabled = False
    , defaultLocale = False
    }

localeListDe :: LocaleList
localeListDe = toLocaleList localeDe

localeDeDto :: LocaleDTO
localeDeDto = (toDTO False localeListDe) {organization = Just orgGlobalSimple}
