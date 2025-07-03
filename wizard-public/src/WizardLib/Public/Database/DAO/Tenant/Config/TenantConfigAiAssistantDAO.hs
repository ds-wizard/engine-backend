module WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigAiAssistantDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigAiAssistant ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

entityName = "config_ai_assistant"

findTenantConfigAiAssistant :: AppContextC s sc m => m TenantConfigAiAssistant
findTenantConfigAiAssistant = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigAiAssistantByUuid tenantUuid

findTenantConfigAiAssistantByUuid :: AppContextC s sc m => U.UUID -> m TenantConfigAiAssistant
findTenantConfigAiAssistantByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigAiAssistant :: AppContextC s sc m => TenantConfigAiAssistant -> m Int64
insertTenantConfigAiAssistant = createInsertFn entityName

updateTenantConfigAiAssistant :: AppContextC s sc m => TenantConfigAiAssistant -> m Int64
updateTenantConfigAiAssistant config = do
  let sql =
        fromString
          "UPDATE config_ai_assistant SET tenant_uuid = ?, enabled = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigAiAssistants :: AppContextC s sc m => m Int64
deleteTenantConfigAiAssistants = createDeleteEntitiesFn entityName
