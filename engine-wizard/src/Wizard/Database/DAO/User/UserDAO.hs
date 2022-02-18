module Wizard.Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.User ()
import Wizard.Database.Mapping.User.UserSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Model.User.UserSuggestion
import Wizard.Service.Cache.UserCache
import Wizard.Util.Cache

entityName = "user_entity"

pageLabel = "users"

findUsers :: AppContextM [User]
findUsers = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page User)
findUsersPage mQuery mRole pageable sort = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "(concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND role ~* ? AND app_uuid = ?"
    [regex mQuery, regex mQuery, regex mRole, U.toString appUuid]

findUserSuggestionsPage ::
     Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
findUserSuggestionsPage mQuery mSelectUuids mExcludeUuids pageable sort = do
  appUuid <- asks _appContextAppUuid
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
          "(concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND active = true AND app_uuid = ? %s %s"
          [selectCondition, excludeCondition]
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, first_name, last_name, email, image_url"
    condition
    ([regex mQuery, regex mQuery, U.toString appUuid] ++ fromMaybe [] mSelectUuids ++ fromMaybe [] mExcludeUuids)

findUserById :: String -> AppContextM User
findUserById = getFromCacheOrDb getFromCache addToCache go
  where
    go uuid = do
      appUuid <- asks _appContextAppUuid
      createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

findUserById' :: String -> AppContextM (Maybe User)
findUserById' = getFromCacheOrDb' getFromCache addToCache go
  where
    go uuid = do
      appUuid <- asks _appContextAppUuid
      createFindEntityByFn' entityName [appQueryUuid appUuid, ("uuid", uuid)]

findUserByEmail :: String -> AppContextM User
findUserByEmail email = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("email", email)]

findUserByEmail' :: String -> AppContextM (Maybe User)
findUserByEmail' email = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("email", email)]

countUsers :: AppContextM Int
countUsers = do
  appUuid <- asks _appContextAppUuid
  createCountByFn entityName appCondition [appUuid]

countActiveUsers :: AppContextM Int
countActiveUsers = do
  appUuid <- asks _appContextAppUuid
  createCountByFn entityName (f' "%s AND active = true" [appCondition]) [appUuid]

insertUser :: User -> AppContextM Int64
insertUser user = do
  result <- createInsertFn entityName user
  addToCache user
  return result

updateUserById :: User -> AppContextM Int64
updateUserById user = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString
          "UPDATE user_entity SET uuid = ?, first_name = ?, last_name = ?, email = ?, password_hash = ?, affiliation = ?, sources = ?, role = ?, permissions = ?, active = ?, submissions_props = ?, image_url = ?, groups = ?, last_visited_at = ?, created_at = ?, updated_at = ?, app_uuid = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow user ++ [toField appUuid, toField $ user ^. uuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  updateCache user
  return result

updateUserPasswordById :: String -> String -> UTCTime -> AppContextM Int64
updateUserPasswordById uUuid uPassword uUpdatedAt = do
  appUuid <- asks _appContextAppUuid
  let sql = fromString "UPDATE user_entity SET password_hash = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toField uPassword, toField uUpdatedAt, toField appUuid, toField uUuid]
  logQuery sql params
  let action conn = execute conn sql params
  result <- runDB action
  deleteFromCache uUuid
  return result

deleteUsers :: AppContextM Int64
deleteUsers = do
  result <- createDeleteEntitiesFn entityName
  deleteAllFromCache
  return result

deleteUserById :: String -> AppContextM Int64
deleteUserById uuid = do
  appUuid <- asks _appContextAppUuid
  result <- createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]
  deleteFromCache uuid
  return result
