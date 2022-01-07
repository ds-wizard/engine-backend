module Database.PostgreSQL.Migration.Database where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.Time
import Database.PostgreSQL.Simple

import Database.PostgreSQL.Migration.Entity

instance ToRow MigrationRecord

instance FromRow MigrationRecord

toMigrationRecord :: MigrationMeta -> MigrationState -> UTCTime -> MigrationRecord
toMigrationRecord migrationMeta state createdAt =
  MigrationRecord
    { mrNumber = mmNumber migrationMeta
    , mrName = mmName migrationMeta
    , mrDescription = mmDescription migrationMeta
    , mrState = state
    , mrCreatedAt = createdAt
    }

getMigrationsFromDb :: Pool Connection -> LoggingT IO [MigrationRecord]
getMigrationsFromDb dbPool = do
  let action conn = query_ conn "SELECT * FROM migration ORDER BY number ASC;"
  runDB dbPool action

ensureMigrationTable :: Pool Connection -> LoggingT IO ()
ensureMigrationTable dbPool = do
  let sql =
        "create table if not exists migration \
            \ ( \
            \     number integer                  not null \
            \         constraint migration_pk \
            \             primary key, \
            \     name            varchar                  not null, \
            \     description     varchar                  not null, \
            \     state           varchar                  not null, \
            \     created_at      timestamp with time zone not null \
            \ ); \
            \ create unique index if not exists migration_number_uindex \
            \    on migration (number); "
  let action conn = execute_ conn sql
  runDB dbPool action
  return ()

startMigration :: Pool Connection -> MigrationMeta -> LoggingT IO ()
startMigration dbPool meta = do
  now <- liftIO getCurrentTime
  let entity = toMigrationRecord meta _STARTED now
  let action conn = execute conn "INSERT INTO migration VALUES (?,?,?,?,?);" entity
  runDB dbPool action
  return ()

endMigration :: Pool Connection -> MigrationMeta -> LoggingT IO ()
endMigration dbPool meta = do
  let action conn = execute conn "UPDATE migration SET state = 'DONE' WHERE number = ?;" [mmNumber meta]
  runDB dbPool action
  return ()

startTransaction :: Pool Connection -> LoggingT IO ()
startTransaction dbPool = do
  let action conn = execute_ conn "BEGIN TRANSACTION;"
  runDB dbPool action
  return ()

commitTransaction :: Pool Connection -> LoggingT IO ()
commitTransaction dbPool = do
  let action conn = execute_ conn "COMMIT;"
  runDB dbPool action
  return ()

rollbackTransaction :: Pool Connection -> LoggingT IO ()
rollbackTransaction dbPool = do
  let action conn = execute_ conn "ROLLBACK;"
  runDB dbPool action
  return ()

runDB dbPool action = liftIO $ withResource dbPool action
