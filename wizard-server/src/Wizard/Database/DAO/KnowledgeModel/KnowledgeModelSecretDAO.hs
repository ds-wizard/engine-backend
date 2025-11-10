module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Secret.KnowledgeModelSecret ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret

entityName = "knowledge_model_secret"

findKnowledgeModelSecrets :: AppContextM [KnowledgeModelSecret]
findKnowledgeModelSecrets = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findKnowledgeModelSecretByUuid :: U.UUID -> AppContextM KnowledgeModelSecret
findKnowledgeModelSecretByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertKnowledgeModelSecret :: KnowledgeModelSecret -> AppContextM Int64
insertKnowledgeModelSecret = createInsertFn entityName

updateKnowledgeModelSecretByUuid :: KnowledgeModelSecret -> AppContextM Int64
updateKnowledgeModelSecretByUuid kmSecret = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE knowledge_model_secret SET uuid = ?, name = ?, value = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow kmSecret ++ [toField tenantUuid, toField kmSecret.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteKnowledgeModelSecrets :: AppContextM Int64
deleteKnowledgeModelSecrets = createDeleteEntitiesFn entityName

deleteKnowledgeModelSecretByUuid :: U.UUID -> AppContextM Int64
deleteKnowledgeModelSecretByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
