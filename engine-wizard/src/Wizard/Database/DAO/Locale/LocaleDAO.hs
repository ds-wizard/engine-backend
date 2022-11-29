module Wizard.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.Mapping.Locale.Locale ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Locale.Locale
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
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleList)
findLocalesPage mOrganizationId mLocaleId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "locale.id, locale.name, locale.description, locale.code, locale.organization_id, locale.locale_id, locale.version, locale.default_locale, locale.enabled, get_locale_state(registry_locale.remote_version, locale.version), registry_locale.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, locale.created_at, locale.updated_at"
    "locale_id"
    mQuery
    Nothing
    mOrganizationId
    mLocaleId
    Nothing
    ""

findLocalesFiltered :: [(String, String)] -> AppContextM [Locale]
findLocalesFiltered queryParams = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

findLocalesByCode :: String -> String -> AppContextM [LocaleSimple]
findLocalesByCode code shortCode = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "SELECT id, \
          \       name, \
          \       code, \
          \       default_locale \
          \FROM locale \
          \WHERE app_uuid = ? \
          \  AND enabled = true \
          \  AND (code = ? \
          \    OR code = ? \
          \    OR default_locale = true);"
  let params = [toField . U.toString $ appUuid, toField code, toField shortCode]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findLocalesByOrganizationIdAndLocaleId :: String -> String -> AppContextM [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleById :: String -> AppContextM Locale
findLocaleById lclId = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", lclId)]

findLocaleById' :: String -> AppContextM (Maybe Locale)
findLocaleById' lclId = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("id", lclId)]

findSimpleLocaleById :: String -> AppContextM LocaleSimple
findSimpleLocaleById lclId = do
  appUuid <- asks currentAppUuid
  createFindEntityWithFieldsByFn "id, name, code, default_locale" False entityName [appQueryUuid appUuid, ("id", lclId)]

updateLocaleById :: Locale -> AppContextM Int64
updateLocaleById locale = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "UPDATE locale SET id = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow locale ++ [toField appUuid, toField locale.lId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultLocale :: AppContextM ()
unsetDefaultLocale = do
  appUuid <- asks currentAppUuid
  let sql = fromString "UPDATE locale SET default_locale = false WHERE app_uuid = ?"
  let params = [toField appUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()

unsetEnabledLocale :: String -> AppContextM ()
unsetEnabledLocale code = do
  appUuid <- asks currentAppUuid
  let sql = fromString "UPDATE locale SET enabled = false WHERE app_uuid = ? AND code = ?"
  let params = [toField appUuid, toField code]
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
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", lclId)]
