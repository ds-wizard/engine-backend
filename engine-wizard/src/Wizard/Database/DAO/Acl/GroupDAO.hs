module Wizard.Database.DAO.Acl.GroupDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Acl.Acl ()
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "acl_group"

findGroups :: AppContextM [Group]
findGroups = createFindEntitiesFn entityName

findGroupById :: String -> AppContextM Group
findGroupById = createFindEntityByFn entityName "id"

insertGroup :: Group -> AppContextM Int64
insertGroup = createInsertFn entityName

deleteGroups :: AppContextM Int64
deleteGroups = createDeleteEntitiesFn entityName
