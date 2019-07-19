module Database.DAO.ActionKey.ActionKeyDAO where

import Data.Bson

import Database.BSON.ActionKey.ActionKey ()
import Database.DAO.Common
import Model.ActionKey.ActionKey
import Model.Context.AppContext
import Model.Error.Error

entityName = "actionKey"

collection = "actionKeys"

findActionKeys :: AppContextM (Either AppError [ActionKey])
findActionKeys = createFindEntitiesFn collection

findActionKeyById :: String -> AppContextM (Either AppError ActionKey)
findActionKeyById = createFindEntityByFn collection entityName "uuid"

findActionKeyByHash :: String -> AppContextM (Either AppError ActionKey)
findActionKeyByHash = createFindEntityByFn collection entityName "hash"

insertActionKey :: ActionKey -> AppContextM Value
insertActionKey = createInsertFn collection

deleteActionKeys :: AppContextM ()
deleteActionKeys = createDeleteEntitiesFn collection

deleteActionKeyById :: String -> AppContextM ()
deleteActionKeyById = createDeleteEntityByFn collection "uuid"

deleteActionKeyByHash :: String -> AppContextM ()
deleteActionKeyByHash = createDeleteEntityByFn collection "hash"
