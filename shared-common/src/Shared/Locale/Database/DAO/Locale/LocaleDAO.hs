module Shared.Locale.Database.DAO.Locale.LocaleDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Database.Mapping.Locale.LocaleSuggestion ()
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSuggestion

entityName = "locale"

pageLabel = "locales"

findLocales :: AppContextC s sc m => m [Locale]
findLocales = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findLocalesByOrganizationIdAndLocaleId :: AppContextC s sc m => String -> String -> m [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleSuggestions :: AppContextC s sc m => Maybe String -> Pageable -> [Sort] -> m (Page LocaleSuggestion)
findLocaleSuggestions mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  let condition = "WHERE (id ~* ? OR name ~* ?) AND enabled = true AND tenant_uuid = ?"
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "id, name, description, code, default_locale"
    condition
    [regexM mQuery, regexM mQuery, U.toString tenantUuid]

findLocaleById :: AppContextC s sc m => String -> m Locale
findLocaleById lclId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", lclId)]

findLocaleById' :: AppContextC s sc m => String -> m (Maybe Locale)
findLocaleById' lclId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("id", lclId)]

findLocaleSuggestionBy :: AppContextC s sc m => [(String, String)] -> m LocaleSuggestion
findLocaleSuggestionBy = createFindEntityWithFieldsByFn "id, name, description, code, default_locale" False entityName

countLocalesGroupedByOrganizationIdAndLocaleId :: AppContextC s sc m => m Int
countLocalesGroupedByOrganizationIdAndLocaleId = do
  tenantUuid <- asks (.tenantUuid')
  countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid

countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant :: AppContextC s sc m => U.UUID -> m Int
countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM locale \
        \      WHERE tenant_uuid = ? \
        \      GROUP BY organization_id, locale_id) nested;"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

updateLocaleById :: AppContextC s sc m => Locale -> m Int64
updateLocaleById locale = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE locale SET id = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND id = ?"
  let params = toRow locale ++ [toField tenantUuid, toField locale.lId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultLocale :: AppContextC s sc m => m ()
unsetDefaultLocale = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE locale SET default_locale = false WHERE tenant_uuid = ?"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

unsetEnabledLocale :: AppContextC s sc m => String -> m ()
unsetEnabledLocale code = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE locale SET enabled = false WHERE tenant_uuid = ? AND code = ?"
  let params = [toField tenantUuid, toField code]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

insertLocale :: AppContextC s sc m => Locale -> m Int64
insertLocale = createInsertFn entityName

deleteLocales :: AppContextC s sc m => m Int64
deleteLocales = createDeleteEntitiesFn entityName

deleteLocaleById :: AppContextC s sc m => String -> m Int64
deleteLocaleById lclId = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", lclId)]
