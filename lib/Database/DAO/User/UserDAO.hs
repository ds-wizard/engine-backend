module Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Time
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)

import Common.Types
import Database.BSON.User.User ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.User.User

userCollection = "users"

findUsers :: AppContextM (Either AppError [User])
findUsers = do
  let action = rest =<< find (select [] userCollection)
  usersS <- runDB action
  return . deserializeEntities $ usersS

findUserById :: String -> AppContextM (Either AppError User)
findUserById userUuid = do
  let action = findOne $ select ["uuid" =: userUuid] userCollection
  maybeUserS <- runDB action
  return . deserializeMaybeEntity $ maybeUserS

findUserByEmail :: Email -> AppContextM (Either AppError User)
findUserByEmail userEmail = do
  let action = findOne $ select ["email" =: userEmail] userCollection
  maybeUserS <- runDB action
  return . deserializeMaybeEntity $ maybeUserS

insertUser :: User -> AppContextM Value
insertUser user = do
  let action = insert userCollection (toBSON user)
  runDB action

updateUserById :: User -> AppContextM ()
updateUserById user = do
  let action = fetch (select ["uuid" =: (user ^. uuid)] userCollection) >>= save userCollection . merge (toBSON user)
  runDB action

updateUserPasswordById :: String -> String -> UTCTime -> AppContextM ()
updateUserPasswordById userUuid password uUpdatedAt = do
  let action =
        modify
          (select ["uuid" =: userUuid] userCollection)
          ["$set" =: ["passwordHash" =: password, "updatedAt" =: uUpdatedAt]]
  runDB action

deleteUsers :: AppContextM ()
deleteUsers = do
  let action = delete $ select [] userCollection
  runDB action

deleteUserById :: String -> AppContextM ()
deleteUserById userUuid = do
  let action = deleteOne $ select ["uuid" =: userUuid] userCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindUsers callback = do
  eitherUser <- findUsers
  case eitherUser of
    Right user -> callback user
    Left error -> return . Left $ error

-- -----------------------------------------------------
heFindUserById userUuid callback = do
  eitherUser <- findUserById userUuid
  case eitherUser of
    Right user -> callback user
    Left error -> return . Left $ error

hmFindUserById userUuid callback = do
  eitherUser <- findUserById userUuid
  case eitherUser of
    Right user -> callback user
    Left error -> return . Just $ error

-- -----------------------------------------------------
heFindUserByEmail userEmail callback = do
  eitherUser <- findUserByEmail userEmail
  case eitherUser of
    Right user -> callback user
    Left error -> return . Left $ error

hmFindUserByEmail userEmail callback = do
  eitherUser <- findUserByEmail userEmail
  case eitherUser of
    Right user -> callback user
    Left error -> return . Just $ error
