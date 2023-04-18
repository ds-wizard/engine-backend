module Registry.Service.Locale.LocaleUtil where

import qualified Data.List as L

import Shared.Common.Util.List (groupBy)
import Shared.Locale.Model.Locale.Locale

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)

groupLocales :: [Locale] -> [[Locale]]
groupLocales =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.localeId == t2.localeId)
