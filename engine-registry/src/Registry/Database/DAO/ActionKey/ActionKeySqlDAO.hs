module Registry.Database.DAO.ActionKey.ActionKeySqlDAO where

import GHC.Int

import Registry.Database.Mapping.ActionKey.ActionKey ()
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.CommonSql

entityName = "action_key"

findActionKeys :: AppContextM [ActionKey]
findActionKeys = createFindEntitiesFn entityName

findActionKeyByHash' :: String -> AppContextM (Maybe ActionKey)
findActionKeyByHash' = createFindEntityByFn' entityName "hash"

insertActionKey :: ActionKey -> AppContextM Int64
insertActionKey = createInsertFn entityName

deleteActionKeys :: AppContextM Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash :: String -> AppContextM Int64
deleteActionKeyByHash = createDeleteEntityByFn entityName "hash"
