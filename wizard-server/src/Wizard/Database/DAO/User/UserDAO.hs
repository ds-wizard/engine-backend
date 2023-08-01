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
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("machine", "false")]

findUsersWithAppFiltered :: U.UUID -> [(String, String)] -> AppContextM [User]
findUsersWithAppFiltered appUuid queryParams =
  createFindEntitiesByFn entityName ([appQueryUuid appUuid, ("machine", "false")] ++ queryParams)

findUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page User)
findUsersPage mQuery mRole pageable sort = do
  appUuid <- asks currentAppUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND role ~* ? AND app_uuid = ? AND machine = false"
    [regex mQuery, regex mQuery, regex mRole, U.toString appUuid]

findUserSuggestionsPage
  :: Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
findUserSuggestionsPage mQuery mSelectUuids mExcludeUuids pageable sort = do
  appUuid <- asks currentAppUuid
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
          "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND active = true AND app_uuid = ? AND machine = false %s %s"
          [selectCondition, excludeCondition]
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, first_name, last_name, email, image_url"
    condition
    ([regex mQuery, regex mQuery, U.toString appUuid] ++ fromMaybe [] mSelectUuids ++ fromMaybe [] mExcludeUuids)

findUserByUuid :: U.UUID -> AppContextM User
findUserByUuid uuid = getFromCacheOrDb getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      appUuid <- asks currentAppUuid
      createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

findUserByUuid' :: U.UUID -> AppContextM (Maybe User)
findUserByUuid' uuid = getFromCacheOrDb' getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      appUuid <- asks currentAppUuid
      createFindEntityByFn' entityName [appQueryUuid appUuid, ("uuid", uuid)]

findUserByUuidSystem' :: U.UUID -> AppContextM (Maybe User)
findUserByUuidSystem' uuid = createFindEntityByFn' entityName [("uuid", U.toString uuid)]

findUserByEmail :: String -> AppContextM User
findUserByEmail email = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("email", email)]

findUserByEmail' :: String -> AppContextM (Maybe User)
findUserByEmail' email = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("email", email)]

findUserByEmailAndAppUuid' :: String -> U.UUID -> AppContextM (Maybe User)
findUserByEmailAndAppUuid' email appUuid = createFindEntityByFn' entityName [appQueryUuid appUuid, ("email", email)]

countUsers :: AppContextM Int
countUsers = do
  appUuid <- asks currentAppUuid
  countUsersWithApp appUuid

countUsersWithApp :: U.UUID -> AppContextM Int
countUsersWithApp appUuid = createCountByFn entityName (f' "%s AND machine = false" [appCondition]) [appUuid]

countActiveUsers :: AppContextM Int
countActiveUsers = do
  appUuid <- asks currentAppUuid
  countActiveUsersWithApp appUuid

countActiveUsersWithApp :: U.UUID -> AppContextM Int
countActiveUsersWithApp appUuid = createCountByFn entityName (f' "%s AND machine = false AND active = true" [appCondition]) [U.toString appUuid]

insertUser :: User -> AppContextM Int64
insertUser user = do
  result <- createInsertFn entityName user
  addToCache user
  return result

updateUserByUuid :: User -> AppContextM Int64
updateUserByUuid user = do
  let sql =
        fromString
          "UPDATE user_entity SET uuid = ?, first_name = ?, last_name = ?, email = ?, password_hash = ?, affiliation = ?, sources = ?, role = ?, permissions = ?, active = ?, submissions_props = ?, image_url = ?, groups = ?, last_visited_at = ?, created_at = ?, updated_at = ?, app_uuid = ?, machine = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow user ++ [toField user.appUuid, toField user.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  updateCache user
  return result

updateUserPasswordByUuid :: U.UUID -> String -> UTCTime -> AppContextM Int64
updateUserPasswordByUuid userUuid uPassword uUpdatedAt = do
  appUuid <- asks currentAppUuid
  let sql = fromString "UPDATE user_entity SET password_hash = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toField uPassword, toField uUpdatedAt, toField appUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  deleteFromCache (U.toString userUuid)
  return result

updateUserLastVisitedAtByUuid :: U.UUID -> UTCTime -> AppContextM Int64
updateUserLastVisitedAtByUuid userUuid lastVisitedAt = do
  appUuid <- asks currentAppUuid
  let sql = fromString "UPDATE user_entity SET last_visited_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toField lastVisitedAt, toField appUuid, toField userUuid]
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
  appUuid <- asks currentAppUuid
  result <- createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
