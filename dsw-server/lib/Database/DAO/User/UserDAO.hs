module Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Time
import Database.MongoDB ((=:), modify, select)

import Database.BSON.User.User ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.User.User
import Util.Helper (createHeeHelper, createHemHelper)

entityName = "user"

collection = "users"

findUsers :: AppContextM (Either AppError [User])
findUsers = createFindEntitiesFn collection

findUserById :: String -> AppContextM (Either AppError User)
findUserById = createFindEntityByFn collection entityName "uuid"

findUserByEmail :: Email -> AppContextM (Either AppError User)
findUserByEmail = createFindEntityByFn collection entityName "email"

countUsers :: AppContextM (Either AppError Int)
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

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindUsers callback = createHeeHelper findUsers callback

-- -----------------------------------------------------
heFindUserById userUuid callback = createHeeHelper (findUserById userUuid) callback

hmFindUserById userUuid callback = createHemHelper (findUserById userUuid) callback

-- -----------------------------------------------------
hmFindUserByEmail userEmail callback = createHemHelper (findUserByEmail userEmail) callback

-- -----------------------------------------------------
heCountUsers callback = createHeeHelper countUsers callback
