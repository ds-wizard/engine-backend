module Wizard.Database.DAO.Level.LevelDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Level.Level ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Level.Level

entityName = "level"

findLevels :: AppContextM [Level]
findLevels = createFindEntitiesFn entityName

insertLevel :: Level -> AppContextM Int64
insertLevel = createInsertFn entityName

deleteLevels :: AppContextM Int64
deleteLevels = createDeleteEntitiesFn entityName

deleteLevelByUuid :: String -> AppContextM Int64
deleteLevelByUuid = createDeleteEntityByFn entityName "uuid"
