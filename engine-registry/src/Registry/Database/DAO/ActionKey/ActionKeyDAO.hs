module Registry.Database.DAO.ActionKey.ActionKeyDAO where

import GHC.Int

import Registry.Database.DAO.Common
import Registry.Database.Mapping.ActionKey.ActionKey ()
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()

entityName = "action_key"

findActionKeys :: AppContextM [ActionKey]
findActionKeys = createFindEntitiesFn entityName

findActionKeyByHash :: String -> AppContextM ActionKey
findActionKeyByHash hash = createFindEntityByFn entityName [("hash", hash)]

insertActionKey :: ActionKey -> AppContextM Int64
insertActionKey = createInsertFn entityName

deleteActionKeys :: AppContextM Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash :: String -> AppContextM Int64
deleteActionKeyByHash hash = createDeleteEntityByFn entityName [("hash", hash)]
