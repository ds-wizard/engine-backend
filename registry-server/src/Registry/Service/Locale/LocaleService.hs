module Registry.Service.Locale.LocaleService where

import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Database.DAO.Locale.LocaleDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Context.AppContext
import Registry.Service.Locale.LocaleMapper
import Registry.Service.Locale.LocaleUtil
import Registry.Service.Locale.LocaleValidation
import RegistryLib.Api.Resource.Locale.LocaleDTO
import Shared.Coordinate.Util.Coordinate
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale

getLocales :: [(String, String)] -> Maybe String -> AppContextM [LocaleDTO]
getLocales queryParams mRecommendedAppVersion = do
  checkIfLocaleEnabled
  locales <- findLocalesFiltered queryParams mRecommendedAppVersion
  orgs <- findOrganizations
  return . fmap (toDTO orgs) . chooseTheNewest . groupLocales $ locales

getLocaleById :: String -> AppContextM LocaleDetailDTO
getLocaleById lclId = do
  checkIfLocaleEnabled
  locale <- findLocaleById lclId
  versions <- getLocaleVersions locale
  org <- findOrganizationByOrgId locale.organizationId
  return $ toDetailDTO locale versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: Locale -> AppContextM [String]
getLocaleVersions locale = do
  allTmls <- findLocalesByOrganizationIdAndLocaleId locale.organizationId locale.localeId
  return . fmap (.version) $ allTmls
