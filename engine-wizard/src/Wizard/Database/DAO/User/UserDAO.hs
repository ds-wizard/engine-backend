module Wizard.Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Time
import Database.MongoDB (modify, select)

import LensesConfig
import Shared.Database.DAO.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.BSON.User.User ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Service.Cache.UserCache

entityName = "user"

collection = "users"

findUsers :: AppContextM [User]
findUsers = createFindEntitiesFn collection

findUsersPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page User)
findUsersPage mQuery pageable sort =
  createAggregateEntitiesPageableQuerySortFn
    collection
    pageable
    sort
    ["$addFields" =: ["name" =: ["$concat" =: ["$firstName", " ", "$lastName"]]]] =<<
  sel [regexSel "name" mQuery]

findUserById :: String -> AppContextM User
findUserById = createFindEntityByFn collection entityName "uuid"

findUserByEmail :: Email -> AppContextM User
findUserByEmail = createFindEntityByFn collection entityName "email"

findUserByEmail' :: Email -> AppContextM (Maybe User)
findUserByEmail' = createFindEntityByFn' collection entityName "email"

countUsers :: AppContextM Int
countUsers = createCountFn collection

insertUser :: User -> AppContextM Value
insertUser user = do
  result <- createInsertFn collection user
  addToCache user
  return result

updateUserById :: User -> AppContextM ()
updateUserById user = do
  createUpdateByFn collection "uuid" (user ^. uuid) user
  updateCache user

updateUserPasswordById :: String -> String -> UTCTime -> AppContextM ()
updateUserPasswordById uUuid uPassword uUpdatedAt = do
  let action =
        modify
          (select ["uuid" =: uUuid] collection)
          ["$set" =: ["passwordHash" =: uPassword, "updatedAt" =: uUpdatedAt]]
  runDB action
  deleteFromCache uUuid

deleteUsers :: AppContextM ()
deleteUsers = do
  createDeleteEntitiesFn collection
  deleteAllFromCache

deleteUserById :: String -> AppContextM ()
deleteUserById uuid = do
  createDeleteEntityByFn collection "uuid" uuid
  deleteFromCache uuid
