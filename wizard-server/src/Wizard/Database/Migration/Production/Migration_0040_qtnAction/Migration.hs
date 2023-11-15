module Wizard.Database.Migration.Production.Migration_0040_qtnAction.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 40, mmName = "Questionnaire Action", mmDescription = "Add Questionnaire Action"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createQtnActionTable dbPool
  addQtnActionPermission dbPool

createQtnActionTable dbPool = do
  let sql =
        "CREATE TABLE questionnaire_action \
        \( \
        \    id                varchar     NOT NULL, \
        \    name              varchar     NOT NULL, \
        \    organization_id   varchar     NOT NULL, \
        \    action_id       varchar     NOT NULL, \
        \    version           varchar     NOT NULL, \
        \    metamodel_version integer     NOT NULL, \
        \    description       varchar     NOT NULL, \
        \    readme            varchar     NOT NULL, \
        \    license           varchar     NOT NULL, \
        \    allowed_packages  json        NOT NULL, \
        \    url               varchar, \
        \    config            json        NOT NULL, \
        \    enabled           bool        NOT NULL, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_action_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_action_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addQtnActionPermission dbPool = do
  let sql =
        "UPDATE user_entity set permissions = permissions || '{QTN_ACTION_PERM}' WHERE role = 'admin' OR role = 'dataSteward'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
