module Wizard.Database.DAO.Acl.GroupDAO where

import Data.Bson

import Shared.Database.DAO.Common
import Wizard.Database.BSON.Acl.Acl ()
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "group"

collection = "groups"

findGroups :: AppContextM [Group]
findGroups = createFindEntitiesFn collection

findGroupById :: String -> AppContextM Group
findGroupById = createFindEntityByFn collection entityName "id"

insertGroup :: Group -> AppContextM Value
insertGroup = createInsertFn collection

deleteGroups :: AppContextM ()
deleteGroups = createDeleteEntitiesFn collection
