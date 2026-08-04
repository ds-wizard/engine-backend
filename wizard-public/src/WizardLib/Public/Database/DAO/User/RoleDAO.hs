module WizardLib.Public.Database.DAO.User.RoleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.User.Role ()
import WizardLib.Public.Database.Mapping.User.RoleList ()
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RoleList

entityName = "role"

pageLabel = "roles"

roleListFields :: String
roleListFields =
  "uuid, name, permissions, (SELECT count(*) FROM user_entity WHERE role_uuid = role.uuid AND tenant_uuid = role.tenant_uuid) AS users_count, is_admin"

findRoles :: AppContextC s sc m => m [Role]
findRoles = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findRolesPage :: AppContextC s sc m => Maybe String -> Pageable -> [Sort] -> m (Page RoleList)
findRolesPage mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    roleListFields
    "WHERE name ~* ? AND tenant_uuid = ?"
    [regexM mQuery, U.toString tenantUuid]

findRoleListByUuid :: AppContextC s sc m => U.UUID -> m RoleList
findRoleListByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn
    roleListFields
    False
    entityName
    [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findRoleByUuid :: AppContextC s sc m => U.UUID -> m Role
findRoleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  findRoleByUuidAndTenant uuid tenantUuid

findRoleByUuidAndTenant :: AppContextC s sc m => U.UUID -> U.UUID -> m Role
findRoleByUuidAndTenant uuid tenantUuid =
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findRoleByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe Role)
findRoleByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertRole :: AppContextC s sc m => Role -> m Int64
insertRole = createInsertFn entityName

updateRoleByUuid :: AppContextC s sc m => Role -> m Int64
updateRoleByUuid role = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE role SET uuid = ?, name = ?, permissions = ?, is_admin = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow role ++ [toField tenantUuid, toField role.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteRoles :: AppContextC s sc m => m Int64
deleteRoles = createDeleteEntitiesFn entityName

deleteRoleByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteRoleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
