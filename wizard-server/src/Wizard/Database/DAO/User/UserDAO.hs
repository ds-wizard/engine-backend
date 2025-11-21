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
import Wizard.Cache.UserCache
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.User ()
import Wizard.Database.Mapping.User.UserSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Model.User.UserSuggestion
import WizardLib.Public.Database.Mapping.User.UserWithMembership ()
import WizardLib.Public.Model.User.UserWithMembership

entityName = "user_entity"

pageLabel = "users"

findUsers :: AppContextM [User]
findUsers = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("machine", "false")]

findUsersFiltered :: [(String, String)] -> AppContextM [User]
findUsersFiltered queryParams = do
  tenantUuid <- asks currentTenantUuid
  findUsersWithTenantFiltered tenantUuid queryParams

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

findUserSuggestionsPage :: Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
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

findUsersByEmails :: [String] -> AppContextM [User]
findUsersByEmails emails = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesInFn entityName tenantUuid "email" emails

findUsersByUserGroupUuid :: U.UUID -> AppContextM [UserWithMembership]
findUsersByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT u.uuid, u.first_name, u.last_name, u.email, u.image_url, ugm.type \
          \FROM user_group_membership ugm \
          \JOIN user_entity u ON u.uuid = ugm.user_uuid AND u.tenant_uuid = ugm.tenant_uuid \
          \WHERE ugm.user_group_uuid = ? AND ugm.tenant_uuid = ? \
          \ORDER BY u.uuid"
  let params = [U.toString userGroupUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findUserByUuid :: U.UUID -> AppContextM User
findUserByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  getFromCacheOrDb getFromCache addToCache go (U.toString uuid, U.toString tenantUuid)
  where
    go (uuid, tenantUuid) = createFindEntityByFn entityName [("tenant_uuid", tenantUuid), ("uuid", uuid)]

findUserByUuid' :: U.UUID -> AppContextM (Maybe User)
findUserByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  getFromCacheOrDb' getFromCache addToCache go (U.toString uuid, U.toString tenantUuid)
  where
    go (uuid, tenantUuid) = createFindEntityByFn' entityName [("tenant_uuid", tenantUuid), ("uuid", uuid)]

findUserByUuidAndTenantUuidSystem :: U.UUID -> U.UUID -> AppContextM User
findUserByUuidAndTenantUuidSystem uuid tenantUuid = createFindEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]

findUserByUuidSystem' :: U.UUID -> U.UUID -> AppContextM (Maybe User)
findUserByUuidSystem' uuid tenantUuid = createFindEntityByFn' entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]

findUserByEmail :: String -> AppContextM User
findUserByEmail email = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("email", email)]

findUserByEmail' :: String -> AppContextM (Maybe User)
findUserByEmail' email = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("email", email), ("machine", "false")]

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
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "INSERT INTO user_entity VALUES (?, ?, ?, ?, ?, ?, ?::varchar[], ?, ?::varchar[], ?, ?, ?, ?, ?, ?, ?, ?)"
  let params = toRow user
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  addToCache user
  return result

updateUserByUuid :: User -> AppContextM Int64
updateUserByUuid user = do
  let sql =
        fromString
          "UPDATE user_entity SET uuid = ?, first_name = ?, last_name = ?, email = ?, password_hash = ?, affiliation = ?, sources = ?, role = ?, permissions = ?, active = ?, image_url = ?, last_visited_at = ?, created_at = ?, updated_at = ?, tenant_uuid = ?, machine = ?, locale = ? WHERE tenant_uuid = ? AND uuid = ?"
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
  deleteFromCache (U.toString userUuid, U.toString tenantUuid)
  return result

updateUserLastVisitedAtByUuid :: U.UUID -> UTCTime -> AppContextM Int64
updateUserLastVisitedAtByUuid userUuid lastVisitedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE user_entity SET last_visited_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField lastVisitedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  deleteFromCache (U.toString userUuid, U.toString tenantUuid)
  return result

updateUserLocaleByUuid :: U.UUID -> Maybe String -> UTCTime -> AppContextM Int64
updateUserLocaleByUuid userUuid mLocale uUpdatedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE user_entity SET locale = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField mLocale, toField uUpdatedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  deleteFromCache (U.toString userUuid, U.toString tenantUuid)
  runDB action

deleteUsers :: AppContextM Int64
deleteUsers = do
  result <- createDeleteEntitiesFn entityName
  deleteAllFromCache
  return result

deleteUserByUuid :: U.UUID -> AppContextM Int64
deleteUserByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  result <- createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid, U.toString tenantUuid)
  return result
