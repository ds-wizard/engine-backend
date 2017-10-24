module Database.DAO.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB (find, findOne, select, insert, fetch, save, merge, deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Types
import Context
import Database.DAO.Common
import Database.Entity.User

userCollection = "users"

findUsers :: Context -> IO [User]
findUsers context = do
  let action = rest =<< find (select [] userCollection)
  users <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) users

findUserById :: Context -> String -> IO (Maybe User)
findUserById context userUuid = do
  let action = findOne $ select ["uuid" =: userUuid] userCollection
  maybeUser <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybeUser of
    Just user -> return . fromBSON $ user
    Nothing -> return Nothing  

findUserByEmail :: Context -> Email -> IO (Maybe User)
findUserByEmail context userEmail = do
  let action = findOne $ select ["email" =: userEmail] userCollection
  maybeUser <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybeUser of
    Just user -> return . fromBSON $ user
    Nothing -> return Nothing  

insertUser :: Context -> User -> IO Value
insertUser context user = do
  let action = insert userCollection (toBSON user)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateUserById :: Context -> User -> IO ()
updateUserById context user = do
  let action = fetch (select ["uuid" =: (user ^. uUuid)] userCollection) >>= save userCollection . merge (toBSON user)
  runMongoDBPoolDef action (context ^. ctxDbPool)
  
deleteUserById :: Context -> String -> IO ()
deleteUserById context userUuid = do
  let action = deleteOne $ select ["uuid" =: userUuid] userCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
  