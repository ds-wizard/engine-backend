module Registry.Database.Migration.Production.Migration_0003_persistentCommand.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 3, mmName = "Persistent Command", mmDescription = "Add persistent command"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "CREATE TABLE persistent_command \
          \ ( \
          \     uuid uuid not null, \
          \     state varchar not null, \
          \     component varchar not null, \
          \     function varchar not null, \
          \     body varchar not null, \
          \     last_error_message varchar, \
          \     attempts int not null, \
          \     max_attempts int not null, \
          \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
          \     created_by varchar not null \
          \       constraint persistent_command_created_by_fk \
          \         references organization, \
          \     created_at timestamptz not null, \
          \     updated_at timestamptz not null \
          \ ); \
          \  \
          \ CREATE unique index persistent_command_uuid_uindex \
          \     on persistent_command (uuid); \
          \ ALTER TABLE persistent_command \
          \     ADD constraint persistent_command_pk \
          \         primary key (uuid); "
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
