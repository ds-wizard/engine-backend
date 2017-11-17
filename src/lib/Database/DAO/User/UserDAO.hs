module Database.DAO.User.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, fetch, save, merge, delete, modify,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Error
import Common.Types
import Common.Context
import Database.BSON.User.User
import Database.DAO.Common
import Model.User.User

userCollection = "users"

findUsers :: Context -> IO (Either AppError [User])
findUsers context = do
  let action = rest =<< find (select [] userCollection)
  usersS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ usersS

findUserById :: Context -> String -> IO (Either AppError User)
findUserById context userUuid = do
  let action = findOne $ select ["uuid" =: userUuid] userCollection
  maybeUserS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeUserS

findUserByEmail :: Context -> Email -> IO (Either AppError User)
findUserByEmail context userEmail = do
  let action = findOne $ select ["email" =: userEmail] userCollection
  maybeUserS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeUserS

insertUser :: Context -> User -> IO Value
insertUser context user = do
  let action = insert userCollection (toBSON user)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateUserById :: Context -> User -> IO ()
updateUserById context user = do
  let action =
        fetch (select ["uuid" =: (user ^. uUuid)] userCollection) >>=
        save userCollection . merge (toBSON user)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateUserPasswordById :: Context -> String -> String -> IO ()
updateUserPasswordById context userUuid password = do
  let action =
        modify
          (select ["uuid" =: userUuid] userCollection)
          ["$set" =: ["passwordHash" =: password]]
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteUsers :: Context -> IO ()
deleteUsers context = do
  let action = delete $ select [] userCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteUserById :: Context -> String -> IO ()
deleteUserById context userUuid = do
  let action = deleteOne $ select ["uuid" =: userUuid] userCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
