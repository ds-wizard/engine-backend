module Wizard.Database.DAO.Tenant.TenantConfigDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.TenantConfig ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "tenant_config"

findCurrentTenantConfig :: AppContextM TenantConfig
findCurrentTenantConfig = do
  tenantUuid <- asks currentTenantUuid
  findCurrentTenantConfigByUuid tenantUuid

findCurrentTenantConfigByUuid :: U.UUID -> AppContextM TenantConfig
findCurrentTenantConfigByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

insertTenantConfig :: TenantConfig -> AppContextM Int64
insertTenantConfig = createInsertFn entityName

updateTenantConfig :: TenantConfig -> AppContextM Int64
updateTenantConfig config = do
  let sql =
        fromString
          "UPDATE tenant_config SET uuid = ?, organization = ?, authentication = ?, privacy_and_support = ?, dashboard_and_login_screen = ?, look_and_feel = ?, registry = ?, knowledge_model = ?, questionnaire = ?, submission = ?, created_at = ?, updated_at = ?, owl = ?, mail_config_uuid = ? WHERE uuid = ?"
  let params = toRow config ++ [toField config.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigs :: AppContextM Int64
deleteTenantConfigs = createDeleteEntitiesFn entityName
