module Database.DAO.ActionKey.ActionKeyDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, find, findOne, insert, rest, select)

import Database.BSON.ActionKey.ActionKey ()
import Database.DAO.Common
import Model.ActionKey.ActionKey
import Model.Context.AppContext
import Model.Error.Error

actionKeyCollection = "actionKeys"

findActionKeys :: AppContextM (Either AppError [ActionKey])
findActionKeys = do
  let action = rest =<< find (select [] actionKeyCollection)
  actionKeysS <- runDB action
  return . deserializeEntities $ actionKeysS

findActionKeyById :: String -> AppContextM (Either AppError ActionKey)
findActionKeyById actionKeyUuid = do
  let action = findOne $ select ["uuid" =: actionKeyUuid] actionKeyCollection
  maybeActionKeyS <- runDB action
  return . deserializeMaybeEntity $ maybeActionKeyS

findActionKeyByHash :: String -> AppContextM (Either AppError ActionKey)
findActionKeyByHash hash = do
  let action = findOne $ select ["hash" =: hash] actionKeyCollection
  maybeActionKeyS <- runDB action
  return . deserializeMaybeEntity $ maybeActionKeyS

insertActionKey :: ActionKey -> AppContextM Value
insertActionKey actionKey = do
  let action = insert actionKeyCollection (toBSON actionKey)
  runDB action

deleteActionKeys :: AppContextM ()
deleteActionKeys = do
  let action = delete $ select [] actionKeyCollection
  runDB action

deleteActionKeyById :: String -> AppContextM ()
deleteActionKeyById actionKeyUuid = do
  let action = deleteOne $ select ["uuid" =: actionKeyUuid] actionKeyCollection
  runDB action

deleteActionKeyByHash :: String -> AppContextM ()
deleteActionKeyByHash hash = do
  let action = deleteOne $ select ["hash" =: hash] actionKeyCollection
  runDB action
