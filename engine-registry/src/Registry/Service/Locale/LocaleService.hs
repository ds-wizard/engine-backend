module Registry.Service.Locale.LocaleService where

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Database.DAO.Locale.LocaleDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Context.AppContext
import Registry.Service.Locale.LocaleMapper
import Registry.Service.Locale.LocaleUtil
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Locale.Locale
import Shared.Util.Coordinate

getLocales :: [(String, String)] -> Maybe String -> AppContextM [LocaleDTO]
getLocales queryParams mRecommendedAppVersion = do
  tmpls <- findLocalesFiltered queryParams mRecommendedAppVersion
  orgs <- findOrganizations
  return . fmap (toDTO orgs) . chooseTheNewest . groupLocales $ tmpls

getLocaleById :: String -> AppContextM LocaleDetailDTO
getLocaleById lclId = do
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
