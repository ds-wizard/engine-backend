module Wizard.Database.Migration.Production.Migration_0056_tour.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 56, mmName = "Add user tours", mmDescription = "Add user tours support"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "CREATE TABLE user_tour \
        \( \
        \    user_uuid               uuid        NOT NULL, \
        \    tour_id                 varchar     NOT NULL, \
        \    tenant_uuid             uuid        NOT NULL, \
        \    created_at              timestamptz NOT NULL, \
        \    CONSTRAINT user_tour_pk PRIMARY KEY (user_uuid, tour_id, tenant_uuid), \
        \    CONSTRAINT user_tour_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_tour_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
