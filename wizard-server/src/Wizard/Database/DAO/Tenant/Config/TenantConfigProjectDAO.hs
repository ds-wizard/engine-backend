module Wizard.Database.DAO.Tenant.Config.TenantConfigProjectDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigProject ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "config_project"

findTenantConfigProject :: AppContextM TenantConfigProject
findTenantConfigProject = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigProjectByUuid tenantUuid

findTenantConfigProjectByUuid :: U.UUID -> AppContextM TenantConfigProject
findTenantConfigProjectByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigProject :: TenantConfigProject -> AppContextM Int64
insertTenantConfigProject = createInsertFn entityName

updateTenantConfigProject :: TenantConfigProject -> AppContextM Int64
updateTenantConfigProject config = do
  let sql =
        fromString
          "UPDATE config_project \
          \SET tenant_uuid = ?, \
          \    visibility_enabled = ?, \
          \    visibility_default_value = ?, \
          \    sharing_enabled = ?, \
          \    sharing_default_value = ?, \
          \    sharing_anonymous_enabled = ?, \
          \    creation = ?, \
          \    project_tagging_enabled = ?, \
          \    project_tagging_tags = ?, \
          \    summary_report = ?, \
          \    feedback_enabled = ?, \
          \    feedback_token = ?, \
          \    feedback_owner = ?, \
          \    feedback_repo = ?, \
          \    created_at = ?, \
          \    updated_at = ? \
          \WHERE tenant_uuid = ?;"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigProjects :: AppContextM Int64
deleteTenantConfigProjects = createDeleteEntitiesFn entityName
