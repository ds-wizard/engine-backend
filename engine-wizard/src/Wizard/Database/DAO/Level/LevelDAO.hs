module Wizard.Database.DAO.Level.LevelDAO where

import Data.Bson

import Shared.Database.DAO.Common
import Wizard.Database.BSON.Level.Level ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Level.Level

entityName = "level"

collection = "levels"

findLevels :: AppContextM [Level]
findLevels = createFindEntitiesFn collection

insertLevel :: Level -> AppContextM Value
insertLevel = createInsertFn collection

deleteLevels :: AppContextM ()
deleteLevels = createDeleteEntitiesFn collection

deleteLevelByUuid :: String -> AppContextM ()
deleteLevelByUuid = createDeleteEntityByFn collection "uuid"
