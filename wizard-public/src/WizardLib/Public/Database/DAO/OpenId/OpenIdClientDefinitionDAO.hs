module WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.OpenId.OpenIdClient ()
import WizardLib.Public.Database.Mapping.OpenId.OpenIdClientSimple ()
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

entityName = "openid_client"

findOpenIdClientDefinitions :: AppContextC s sc m => m [OpenIdClient]
findOpenIdClientDefinitions = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findOpenIdClientDefinitionsSimpleByTenantUuid :: AppContextC s sc m => U.UUID -> m [OpenIdClientSimple]
findOpenIdClientDefinitionsSimpleByTenantUuid tenantUuid =
  createFindEntitiesWithFieldsByFn "uuid, name, url, style" entityName [tenantQueryUuid tenantUuid]

findOpenIdClientDefinitionByUuid :: AppContextC s sc m => U.UUID -> m OpenIdClient
findOpenIdClientDefinitionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findOpenIdClientDefinitionByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe OpenIdClient)
findOpenIdClientDefinitionByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findOpenIdClientDefinitionByUuidAndTenantUuid' :: AppContextC s sc m => U.UUID -> U.UUID -> m (Maybe OpenIdClient)
findOpenIdClientDefinitionByUuidAndTenantUuid' uuid tenantUuid =
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertOpenIdClientDefinition :: AppContextC s sc m => OpenIdClient -> m Int64
insertOpenIdClientDefinition = createInsertFn entityName

updateOpenIdClientDefinition :: AppContextC s sc m => OpenIdClient -> m Int64
updateOpenIdClientDefinition openIdClient = do
  let sql =
        fromString
          "UPDATE openid_client SET uuid = ?, name = ?, url = ?, client_id = ?, client_secret = ?, parameters = ?, style = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, registration_enabled = ?, scope_profile = ?, scope_email = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow openIdClient ++ [toField openIdClient.uuid, toField openIdClient.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteOpenIdClientDefinitionDefinitions :: AppContextC s sc m => m Int64
deleteOpenIdClientDefinitionDefinitions = createDeleteEntitiesFn entityName

deleteOpenIdClientDefinitionByUuid :: AppContextC s sc m => U.UUID -> m ()
deleteOpenIdClientDefinitionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
