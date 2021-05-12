module Wizard.Database.DAO.User.UserDAO where

import Control.Lens ((^.))
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
findUsers = createFindEntitiesFn entityName

findUsersPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page User)
findUsersPage mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "concat(first_name, ' ', last_name) ~* ?"
    [regex mQuery]

findUserSuggestionsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
findUserSuggestionsPage mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, first_name, last_name, email, image_url"
    "concat(first_name, ' ', last_name) ~* ?"
    [regex mQuery]

findUserById :: String -> AppContextM User
findUserById = getFromCacheOrDb getFromCache addToCache go
  where
    go = createFindEntityByFn entityName "uuid"

findUserByEmail :: String -> AppContextM User
findUserByEmail = createFindEntityByFn entityName "email"

findUserByEmail' :: String -> AppContextM (Maybe User)
findUserByEmail' = createFindEntityByFn' entityName "email"

countUsers :: AppContextM Int
countUsers = createCountFn entityName

insertUser :: User -> AppContextM Int64
insertUser user = do
  result <- createInsertFn entityName user
  addToCache user
  return result

updateUserById :: User -> AppContextM Int64
updateUserById user = do
  let params = toRow user ++ [toField . U.toText $ user ^. uuid]
  let action conn =
        execute
          conn
          "UPDATE user_entity SET uuid = ?, first_name = ?, last_name = ?, email = ?, password_hash = ?, affiliation = ?, sources = ?, role = ?, permissions = ?, active = ?, submissions_props = ?, image_url = ?, groups = ?, last_visited_at = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
          params
  result <- runDB action
  updateCache user
  return result

updateUserPasswordById :: String -> String -> UTCTime -> AppContextM Int64
updateUserPasswordById uUuid uPassword uUpdatedAt = do
  let params = [toField uPassword, toField uUpdatedAt, toField uUuid]
  let action conn = execute conn "UPDATE user_entity SET password_hash = ?, updated_at = ? WHERE uuid = ?" params
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
  result <- createDeleteEntityByFn entityName "uuid" uuid
  deleteFromCache uuid
  return result
