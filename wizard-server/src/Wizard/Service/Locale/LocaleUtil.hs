module Wizard.Service.Locale.LocaleUtil where

import qualified Data.List as L

selectLocaleByOrgIdAndLocaleId locale =
  L.find (\l -> l.organizationId == locale.organizationId && l.localeId == locale.localeId)

selectOrganizationByOrgId locale = L.find (\org -> org.organizationId == locale.organizationId)
