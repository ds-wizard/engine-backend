module Database.DAO.UserDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB (find, findOne, select, insert, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Context
import Database.DAO.Common
import Database.Entity.User

findUsers :: Context -> IO [User]
findUsers context = do
  let action = rest =<< find (select [] "users")
  users <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) users

insertUser :: Context -> User -> IO Value
insertUser context user = do
  let action = insert "users" (toBSON user)
  runMongoDBPoolDef action (context ^. ctxDbPool)

findUserById :: Context -> String -> IO (Maybe User)
findUserById context userUuid = do
  let action = findOne $ select ["uuid" =: userUuid] "users"
  maybeUser <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybeUser of
    Just user -> return . fromBSON $ user
    Nothing -> return Nothing
