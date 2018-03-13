module Database.DAO.ActionKey.ActionKeyDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       ((=:), delete, deleteOne, find, findOne, insert, rest, select)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Common.Types
import Database.BSON.ActionKey.ActionKey
import Database.DAO.Common
import Model.ActionKey.ActionKey

actionKeyCollection = "actionKeys"

findActionKeys :: Context -> IO (Either AppError [ActionKey])
findActionKeys context = do
  let action = rest =<< find (select [] actionKeyCollection)
  actionKeysS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ actionKeysS

findActionKeyById :: Context -> String -> IO (Either AppError ActionKey)
findActionKeyById context actionKeyUuid = do
  let action = findOne $ select ["uuid" =: actionKeyUuid] actionKeyCollection
  maybeActionKeyS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeActionKeyS

findActionKeyByHash :: Context -> String -> IO (Either AppError ActionKey)
findActionKeyByHash context hash = do
  let action = findOne $ select ["hash" =: hash] actionKeyCollection
  maybeActionKeyS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeActionKeyS

insertActionKey :: Context -> ActionKey -> IO Value
insertActionKey context actionKey = do
  let action = insert actionKeyCollection (toBSON actionKey)
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteActionKeys :: Context -> IO ()
deleteActionKeys context = do
  let action = delete $ select [] actionKeyCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteActionKeyById :: Context -> String -> IO ()
deleteActionKeyById context actionKeyUuid = do
  let action = deleteOne $ select ["uuid" =: actionKeyUuid] actionKeyCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteActionKeyByHash :: Context -> String -> IO ()
deleteActionKeyByHash context hash = do
  let action = deleteOne $ select ["hash" =: hash] actionKeyCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
