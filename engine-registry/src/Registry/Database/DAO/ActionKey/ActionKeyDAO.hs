module Registry.Database.DAO.ActionKey.ActionKeyDAO where

import Data.Bson

import Registry.Database.BSON.ActionKey.ActionKey ()
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.Common

entityName = "actionKey"

collection = "actionKeys"

findActionKeys :: AppContextM [ActionKey]
findActionKeys = createFindEntitiesFn collection

findActionKeyById :: String -> AppContextM ActionKey
findActionKeyById = createFindEntityByFn collection entityName "uuid"

findActionKeyByHash' :: String -> AppContextM (Maybe ActionKey)
findActionKeyByHash' = createFindEntityByFn' collection entityName "hash"

insertActionKey :: ActionKey -> AppContextM Value
insertActionKey = createInsertFn collection

deleteActionKeys :: AppContextM ()
deleteActionKeys = createDeleteEntitiesFn collection

deleteActionKeyById :: String -> AppContextM ()
deleteActionKeyById = createDeleteEntityByFn collection "uuid"

deleteActionKeyByHash :: String -> AppContextM ()
deleteActionKeyByHash = createDeleteEntityByFn collection "hash"
