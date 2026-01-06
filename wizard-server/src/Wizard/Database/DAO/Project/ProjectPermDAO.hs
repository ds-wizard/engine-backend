module Wizard.Database.DAO.Project.ProjectPermDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Project.ProjectPerm ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Acl.ProjectPerm

entityName_user = "project_perm_user"

entityName_group = "project_perm_group"

findProjectPermsFiltered :: [(String, String)] -> AppContextM [ProjectPerm]
findProjectPermsFiltered queryParams = do
  let sql =
        fromString $
          f'
            "SELECT project_uuid, 'UserProjectPermType' AS member_type, user_uuid as member_uuid, perms, tenant_uuid \
            \ FROM %s \
            \ WHERE %s \
            \ UNION \
            \ SELECT project_uuid, 'UserGroupProjectPermType' AS member_type, user_group_uuid as member_uuid, perms, tenant_uuid \
            \ FROM %s \
            \ WHERE %s"
            [entityName_user, mapToDBQuerySql queryParams, entityName_group, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams ++ fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertProjectPerm :: ProjectPerm -> AppContextM Int64
insertProjectPerm perm =
  case perm.memberType of
    UserProjectPermType -> createInsertFn entityName_user perm
    UserGroupProjectPermType -> createInsertFn entityName_group perm

deleteProjectPerms :: AppContextM Int64
deleteProjectPerms = do
  createDeleteEntitiesFn entityName_user
  createDeleteEntitiesFn entityName_group

deleteProjectPermsFiltered :: [(String, String)] -> AppContextM Int64
deleteProjectPermsFiltered queryParams = do
  createDeleteEntitiesByFn entityName_user queryParams
  createDeleteEntitiesByFn entityName_group queryParams

deleteProjectPermGroupByUserGroupUuid :: U.UUID -> AppContextM Int64
deleteProjectPermGroupByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName_group [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)]
