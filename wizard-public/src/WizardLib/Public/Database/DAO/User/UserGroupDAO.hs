module WizardLib.Public.Database.DAO.User.UserGroupDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.User.UserGroup ()
import WizardLib.Public.Model.User.UserGroup

entityName = "user_group"

pageLabel = "userGroups"

createFindUserGroupPage :: (AppContextC s sc m, FromRow userGroup) => String -> U.UUID -> Bool -> Maybe String -> String -> Pageable -> [Sort] -> m (Page userGroup)
createFindUserGroupPage fields currentUserUuid isAdmin mQuery additionalCondition pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (nameCondition, nameRegex) =
          case mQuery of
            Just query -> (" AND ug.name ~* ?", [regex query])
            Nothing -> ("", [])
    let (aclJoins, aclCondition) =
          if isAdmin
            then ("", "")
            else
              ( "LEFT JOIN user_group_membership ugm ON ugm.user_group_uuid = ug.uuid AND ugm.tenant_uuid = ug.tenant_uuid"
              , f' "AND (ug.private IS FALSE OR ugm.user_uuid = '${userUuid}')" [U.toString currentUserUuid]
              )
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT COUNT(DISTINCT ug.uuid) \
              \FROM user_group ug \
              \${aclJoin} \
              \WHERE ug.tenant_uuid = '${tenantUuid}' ${nameCondition} ${aclCondition} ${additionalCondition}"
              [ ("userUuid", U.toString currentUserUuid)
              , ("aclJoin", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("nameCondition", nameCondition)
              , ("aclCondition", aclCondition)
              , ("additionalCondition", additionalCondition)
              ]
    let params = nameRegex
    logQuery countSql params
    let action conn = query conn countSql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT DISTINCT ${fields} \
              \FROM user_group ug \
              \${aclJoin} \
              \WHERE ug.tenant_uuid = '${tenantUuid}' ${nameCondition} ${aclCondition} ${additionalCondition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("fields", fields)
              , ("userUuid", U.toString currentUserUuid)
              , ("aclJoin", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("nameCondition", nameCondition)
              , ("aclCondition", aclCondition)
              , ("additionalCondition", additionalCondition)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findUserGroupByUuid :: AppContextC s sc m => U.UUID -> m UserGroup
findUserGroupByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertUserGroup :: AppContextC s sc m => UserGroup -> m Int64
insertUserGroup = createInsertFn entityName

updateUserGroupByUuid :: AppContextC s sc m => UserGroup -> m Int64
updateUserGroupByUuid userGroup = do
  let sql =
        fromString
          "UPDATE user_group SET uuid = ?, name = ?, description = ?, private = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow userGroup ++ [toField userGroup.uuid, toField userGroup.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserGroups :: AppContextC s sc m => m Int64
deleteUserGroups = createDeleteEntitiesFn entityName

deleteUserGroupByUuid :: AppContextC s sc m => U.UUID -> m ()
deleteUserGroupByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
