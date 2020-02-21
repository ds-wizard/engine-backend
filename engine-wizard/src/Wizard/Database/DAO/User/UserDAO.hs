module Wizard.Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Time
import Database.MongoDB ((=:), modify, select)

import LensesConfig
import Wizard.Database.BSON.User.User ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

entityName = "user"

collection = "users"

findUsers :: AppContextM [User]
findUsers = createFindEntitiesFn collection

findUserById :: String -> AppContextM User
findUserById = createFindEntityByFn collection entityName "uuid"

findUserByEmail :: Email -> AppContextM User
findUserByEmail = createFindEntityByFn collection entityName "email"

findUserByEmail' :: Email -> AppContextM (Maybe User)
findUserByEmail' = createFindEntityByFn' collection entityName "email"

countUsers :: AppContextM Int
countUsers = createCountFn collection

insertUser :: User -> AppContextM Value
insertUser = createInsertFn collection

updateUserById :: User -> AppContextM ()
updateUserById user = createUpdateByFn collection "uuid" (user ^. uuid) user

updateUserPasswordById :: String -> String -> UTCTime -> AppContextM ()
updateUserPasswordById uUuid uPassword uUpdatedAt = do
  let action =
        modify
          (select ["uuid" =: uUuid] collection)
          ["$set" =: ["passwordHash" =: uPassword, "updatedAt" =: uUpdatedAt]]
  runDB action

deleteUsers :: AppContextM ()
deleteUsers = createDeleteEntitiesFn collection

deleteUserById :: String -> AppContextM ()
deleteUserById = createDeleteEntityByFn collection "uuid"
