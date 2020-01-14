module Wizard.Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Time
import Database.MongoDB ((=:), modify, select)

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper, createHemHelper)
import Wizard.Database.BSON.User.User ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

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
