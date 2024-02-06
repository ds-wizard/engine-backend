module Wizard.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Model.Locale.Locale
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Locale.LocaleList ()
import Wizard.Database.Mapping.Locale.LocaleSimple ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Locale.LocaleList
import Wizard.Model.Locale.LocaleSimple

entityName = "locale"

pageLabel = "locales"

findLocales :: AppContextM [Locale]
findLocales = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleList)
findLocalesPage mOrganizationId mLocaleId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "locale.id, locale.name, locale.description, locale.code, locale.organization_id, locale.locale_id, locale.version, locale.default_locale, locale.enabled, registry_locale.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, locale.created_at, locale.updated_at"
    "locale_id"
    mQuery
    Nothing
    mOrganizationId
    mLocaleId
    Nothing
    ""

findLocalesFiltered :: [(String, String)] -> AppContextM [Locale]
findLocalesFiltered queryParams = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findLocalesFilteredWithTenant :: U.UUID -> [(String, String)] -> AppContextM [Locale]
findLocalesFilteredWithTenant tenantUuid queryParams = createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findLocalesByCodeWithTenant :: U.UUID -> String -> String -> AppContextM [LocaleSimple]
findLocalesByCodeWithTenant tenantUuid code shortCode = do
  let sql =
        fromString
          "SELECT id, name, code, default_locale \
          \FROM locale \
          \WHERE tenant_uuid = ? \
          \  AND enabled = true \
          \  AND (code = ? \
          \    OR code = ? \
          \    OR default_locale = true);"
  let params = [toField . U.toString $ tenantUuid, toField code, toField shortCode]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findLocalesByOrganizationIdAndLocaleId :: String -> String -> AppContextM [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleById :: String -> AppContextM Locale
findLocaleById lclId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", lclId)]

findLocaleById' :: String -> AppContextM (Maybe Locale)
findLocaleById' lclId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("id", lclId)]

findSimpleLocaleByIdWithTenant :: U.UUID -> String -> AppContextM LocaleSimple
findSimpleLocaleByIdWithTenant tenantUuid lclId =
  createFindEntityWithFieldsByFn "id, name, code, default_locale" False entityName [tenantQueryUuid tenantUuid, ("id", lclId)]

updateLocaleById :: Locale -> AppContextM Int64
updateLocaleById locale = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE locale SET id = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND id = ?"
  let params = toRow locale ++ [toField tenantUuid, toField locale.lId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultLocale :: AppContextM ()
unsetDefaultLocale = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE locale SET default_locale = false WHERE tenant_uuid = ?"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()

unsetEnabledLocale :: String -> AppContextM ()
unsetEnabledLocale code = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE locale SET enabled = false WHERE tenant_uuid = ? AND code = ?"
  let params = [toField tenantUuid, toField code]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()

insertLocale :: Locale -> AppContextM Int64
insertLocale = createInsertFn entityName

deleteLocales :: AppContextM Int64
deleteLocales = createDeleteEntitiesFn entityName

deleteLocaleById :: String -> AppContextM Int64
deleteLocaleById lclId = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", lclId)]
