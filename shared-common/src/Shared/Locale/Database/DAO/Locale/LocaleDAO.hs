module Shared.Locale.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Model.Locale.Locale

entityName = "locale"

pageLabel = "locales"

findLocales :: AppContextC s sc m => m [Locale]
findLocales = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findLocalesByOrganizationIdAndLocaleId :: AppContextC s sc m => String -> String -> m [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleById :: AppContextC s sc m => String -> m Locale
findLocaleById lclId = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", lclId)]

findLocaleById' :: AppContextC s sc m => String -> m (Maybe Locale)
findLocaleById' lclId = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("id", lclId)]

countLocalesGroupedByOrganizationIdAndLocaleId :: AppContextC s sc m => m Int
countLocalesGroupedByOrganizationIdAndLocaleId = do
  appUuid <- asks (.appUuid')
  countLocalesGroupedByOrganizationIdAndLocaleIdWithApp appUuid

countLocalesGroupedByOrganizationIdAndLocaleIdWithApp :: AppContextC s sc m => U.UUID -> m Int
countLocalesGroupedByOrganizationIdAndLocaleIdWithApp appUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM locale \
        \      WHERE app_uuid = ? \
        \      GROUP BY organization_id, locale_id) nested;"
  let params = [U.toString appUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

updateLocaleById :: AppContextC s sc m => Locale -> m Int64
updateLocaleById locale = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE locale SET id = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow locale ++ [toField appUuid, toField locale.lId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

insertLocale :: AppContextC s sc m => Locale -> m Int64
insertLocale = createInsertFn entityName

deleteLocales :: AppContextC s sc m => m Int64
deleteLocales = createDeleteEntitiesFn entityName

deleteLocaleById :: AppContextC s sc m => String -> m Int64
deleteLocaleById lclId = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", lclId)]
