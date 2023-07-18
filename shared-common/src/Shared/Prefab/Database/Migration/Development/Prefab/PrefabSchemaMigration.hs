module Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int
import GHC.Records

import Shared.Common.Constant.Component
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger

runMigration
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Prefab) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Prefab) ended"

dropTables
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) drop table"
  let sql = "drop table if exists prefab cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) create table"
  let sql =
        "create table prefab \
        \ ( \
        \     uuid              uuid              not null, \
        \     type              varchar           not null,\
        \     name              varchar           not null,\
        \     content           json              not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint prefab_app_uuid_fk \
        \         references app, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null, \
        \     constraint prefab_pk \
        \        primary key (uuid, app_uuid) \
        \ ); \
        \  \
        \ create unique index prefab_uuid_uindex \
        \     on prefab (uuid, app_uuid);"
  let action conn = execute_ conn sql
  runDB action
