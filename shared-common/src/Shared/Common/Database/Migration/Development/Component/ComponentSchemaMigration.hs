module Shared.Common.Database.Migration.Development.Component.ComponentSchemaMigration where

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
  logInfo _CMP_MIGRATION "(Table/Component) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Component) ended"

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
  logInfo _CMP_MIGRATION "(Table/Component) drop table"
  let sql = "drop table if exists component cascade;"
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
  logInfo _CMP_MIGRATION "(Table/Component) create table"
  let sql =
        "create table component \
        \ ( \
        \     name                      varchar not null \
        \         constraint component_pk \
        \             primary key, \
        \     version                   varchar not null,\
        \     built_at timestamp with time zone not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ create unique index component_name_uindex \
        \     on component (name);"
  let action conn = execute_ conn sql
  runDB action
