module Registry.Database.DAO.ActionKey.ActionKeyDAO where

import Data.Bson

import Registry.Database.BSON.ActionKey.ActionKey ()
import Registry.Database.DAO.Common
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Shared.Model.Error.Error

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
