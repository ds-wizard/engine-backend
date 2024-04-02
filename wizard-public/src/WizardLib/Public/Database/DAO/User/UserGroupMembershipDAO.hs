module WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.User.UserGroupMembership ()
import WizardLib.Public.Model.User.UserGroupMembership

entityName = "user_group_membership"

findUserGroupMembershipsByUserUuid :: AppContextC s sc m => U.UUID -> m [UserGroupMembership]
findUserGroupMembershipsByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserGroupMembershipsByUserGroupUuid :: AppContextC s sc m => U.UUID -> m [UserGroupMembership]
findUserGroupMembershipsByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)] [Sort "user_uuid" Ascending]

findUserGroupMembershipByUserGroupUuidAndUserUuid' :: AppContextC s sc m => U.UUID -> U.UUID -> m (Maybe UserGroupMembership)
findUserGroupMembershipByUserGroupUuidAndUserUuid' userGroupUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid), ("user_uuid", U.toString userUuid)]

insertUserGroupMembership :: AppContextC s sc m => UserGroupMembership -> m Int64
insertUserGroupMembership = createInsertFn entityName

updateUserGroupMembershipByUuid :: AppContextC s sc m => UserGroupMembership -> m Int64
updateUserGroupMembershipByUuid userGroupMembership = do
  let sql =
        fromString
          "UPDATE user_group_membership SET user_group_uuid = ?, user_uuid = ?, type = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE user_group_uuid = ? AND user_uuid = ? AND tenant_uuid = ?"
  let params = toRow userGroupMembership ++ [toField userGroupMembership.userGroupUuid, toField userGroupMembership.userUuid, toField userGroupMembership.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserGroupMemberships :: AppContextC s sc m => m Int64
deleteUserGroupMemberships = createDeleteEntitiesFn entityName

deleteUserGroupMembershipsByUserGroupUuid :: AppContextC s sc m => U.UUID -> m ()
deleteUserGroupMembershipsByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)]
  return ()

deleteUserGroupMembershipsByUserUuid :: AppContextC s sc m => U.UUID -> m ()
deleteUserGroupMembershipsByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]
  return ()

deleteUserGroupMembershipsByUserGroupUuidAndUserUuids :: AppContextC s sc m => U.UUID -> [U.UUID] -> m ()
deleteUserGroupMembershipsByUserGroupUuidAndUserUuids userGroupUuid userUuids = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE FROM %s WHERE tenant_uuid = ? AND user_group_uuid = ? AND user_uuid IN (%s)"
            [entityName, generateQuestionMarks userUuids]
  let params = fmap toField (tenantUuid : userGroupUuid : userUuids)
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()
