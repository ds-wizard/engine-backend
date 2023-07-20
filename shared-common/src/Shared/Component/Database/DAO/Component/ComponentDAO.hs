module Shared.Component.Database.DAO.Component.ComponentDAO where

import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Database.Mapping.Component.Component ()
import Shared.Common.Model.Context.AppContext
import Shared.Component.Model.Component.Component

entityName = "component"

pageLabel = "components"

findComponents :: AppContextC s sc m => m [Component]
findComponents = createFindEntitiesFn entityName

insertComponent :: AppContextC s sc m => Component -> m Int64
insertComponent = createInsertFn entityName

deleteComponents :: AppContextC s sc m => m Int64
deleteComponents = createDeleteEntitiesFn entityName
