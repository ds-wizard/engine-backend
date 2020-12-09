module Wizard.Database.DAO.ActionKey.ActionKeyDAO where

import Data.Bson

import Shared.Database.DAO.Common
import Wizard.Database.BSON.ActionKey.ActionKey ()
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
