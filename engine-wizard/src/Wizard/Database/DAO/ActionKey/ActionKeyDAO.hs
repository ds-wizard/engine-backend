module Wizard.Database.DAO.ActionKey.ActionKeyDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.ActionKey.ActionKey ()
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
