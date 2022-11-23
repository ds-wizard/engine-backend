module Registry.Service.Locale.LocaleUtil where

import qualified Data.List as L

import Shared.Model.Locale.Locale
import Shared.Util.List (groupBy)

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)

groupLocales :: [Locale] -> [[Locale]]
groupLocales =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.localeId == t2.localeId)
