module Wizard.Database.DAO.User.UserDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Cache
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.User ()
import Wizard.Database.Mapping.User.UserSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Model.User.UserSuggestion
import Wizard.Service.Cache.UserCache

entityName = "user_entity"

pageLabel = "users"

findUsers :: AppContextM [User]
findUsers = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("machine", "false")]

findUsersWithTenantFiltered :: U.UUID -> [(String, String)] -> AppContextM [User]
findUsersWithTenantFiltered tenantUuid queryParams =
  createFindEntitiesByFn entityName ([tenantQueryUuid tenantUuid, ("machine", "false")] ++ queryParams)

findUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page User)
findUsersPage mQuery mRole pageable sort = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND role ~* ? AND tenant_uuid = ? AND machine = false"
    [regexM mQuery, regexM mQuery, regexM mRole, U.toString tenantUuid]

findUserSuggestionsPage
  :: Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
findUserSuggestionsPage mQuery mSelectUuids mExcludeUuids pageable sort = do
  tenantUuid <- asks currentTenantUuid
  let selectCondition =
        case mSelectUuids of
          Nothing -> ""
          Just [] -> ""
          Just selectUuids -> f' "AND uuid IN (%s)" [generateQuestionMarks selectUuids]
  let excludeCondition =
        case mExcludeUuids of
          Nothing -> ""
          Just [] -> ""
          Just excludeUuids -> f' "AND uuid NOT IN (%s)" [generateQuestionMarks excludeUuids]
  let condition =
        f'
          "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND active = true AND tenant_uuid = ? AND machine = false %s %s"
          [selectCondition, excludeCondition]
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, first_name, last_name, email, image_url"
    condition
    ([regexM mQuery, regexM mQuery, U.toString tenantUuid] ++ fromMaybe [] mSelectUuids ++ fromMaybe [] mExcludeUuids)

findUserByUuid :: U.UUID -> AppContextM User
findUserByUuid uuid = getFromCacheOrDb getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      tenantUuid <- asks currentTenantUuid
      createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", uuid)]

findUserByUuid' :: U.UUID -> AppContextM (Maybe User)
findUserByUuid' uuid = getFromCacheOrDb' getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      tenantUuid <- asks currentTenantUuid
      createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", uuid)]

findUserByUuidAndTenantUuidSystem :: U.UUID -> U.UUID -> AppContextM User
findUserByUuidAndTenantUuidSystem uuid tenantUuid = createFindEntityByFn entityName [("uuid", U.toString uuid), ("tenantUuid", U.toString tenantUuid)]

findUserByUuidSystem' :: U.UUID -> AppContextM (Maybe User)
findUserByUuidSystem' uuid = createFindEntityByFn' entityName [("uuid", U.toString uuid)]

findUserByEmail :: String -> AppContextM User
findUserByEmail email = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("email", email)]

findUserByEmail' :: String -> AppContextM (Maybe User)
findUserByEmail' email = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("email", email)]

findUserByEmailAndTenantUuid' :: String -> U.UUID -> AppContextM (Maybe User)
findUserByEmailAndTenantUuid' email tenantUuid = createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("email", email)]

countUsers :: AppContextM Int
countUsers = do
  tenantUuid <- asks currentTenantUuid
  countUsersWithTenant tenantUuid

countUsersWithTenant :: U.UUID -> AppContextM Int
countUsersWithTenant tenantUuid = createCountByFn entityName (f' "%s AND machine = false" [tenantCondition]) [tenantUuid]

countActiveUsers :: AppContextM Int
countActiveUsers = do
  tenantUuid <- asks currentTenantUuid
  countActiveUsersWithTenant tenantUuid

countActiveUsersWithTenant :: U.UUID -> AppContextM Int
countActiveUsersWithTenant tenantUuid = createCountByFn entityName (f' "%s AND machine = false AND active = true" [tenantCondition]) [U.toString tenantUuid]

insertUser :: User -> AppContextM Int64
insertUser user = do
  result <- createInsertFn entityName user
  addToCache user
  return result

updateUserByUuid :: User -> AppContextM Int64
updateUserByUuid user = do
  let sql =
        fromString
          "UPDATE user_entity SET uuid = ?, first_name = ?, last_name = ?, email = ?, password_hash = ?, affiliation = ?, sources = ?, role = ?, permissions = ?, active = ?, submissions_props = ?, image_url = ?, groups = ?, last_visited_at = ?, created_at = ?, updated_at = ?, tenant_uuid = ?, machine = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow user ++ [toField user.tenantUuid, toField user.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  updateCache user
  return result

updateUserPasswordByUuid :: U.UUID -> String -> UTCTime -> AppContextM Int64
updateUserPasswordByUuid userUuid uPassword uUpdatedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE user_entity SET password_hash = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField uPassword, toField uUpdatedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  deleteFromCache (U.toString userUuid)
  return result

updateUserLastVisitedAtByUuid :: U.UUID -> UTCTime -> AppContextM Int64
updateUserLastVisitedAtByUuid userUuid lastVisitedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE user_entity SET last_visited_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField lastVisitedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  deleteFromCache (U.toString userUuid)
  return result

deleteUsers :: AppContextM Int64
deleteUsers = do
  result <- createDeleteEntitiesFn entityName
  deleteAllFromCache
  return result

deleteUserByUuid :: U.UUID -> AppContextM Int64
deleteUserByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  result <- createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
