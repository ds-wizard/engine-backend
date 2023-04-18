module Shared.Locale.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Model.Locale.Locale

entityName = "locale"

pageLabel = "locales"

findLocales
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m [Locale]
findLocales = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findLocalesByOrganizationIdAndLocaleId
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> String
  -> m [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleById
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m Locale
findLocaleById lclId = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", lclId)]

findLocaleById'
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m (Maybe Locale)
findLocaleById' lclId = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("id", lclId)]

countLocalesGroupedByOrganizationIdAndLocaleId
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int
countLocalesGroupedByOrganizationIdAndLocaleId = do
  appUuid <- asks (.appUuid')
  countLocalesGroupedByOrganizationIdAndLocaleIdWithApp appUuid

countLocalesGroupedByOrganizationIdAndLocaleIdWithApp
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m Int
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

updateLocaleById
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => Locale
  -> m Int64
updateLocaleById locale = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE locale SET id = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow locale ++ [toField appUuid, toField locale.lId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

insertLocale
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => Locale
  -> m Int64
insertLocale = createInsertFn entityName

deleteLocales
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
deleteLocales = createDeleteEntitiesFn entityName

deleteLocaleById
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m Int64
deleteLocaleById lclId = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", lclId)]
