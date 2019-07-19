module Database.DAO.Level.LevelDAO where

import Data.Bson

import Database.BSON.Level.Level ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Level.Level
import Util.Helper (createHeeHelper)

entityName = "level"

collection = "levels"

findLevels :: AppContextM (Either AppError [Level])
findLevels = createFindEntitiesFn collection

insertLevel :: Level -> AppContextM Value
insertLevel = createInsertFn collection

deleteLevels :: AppContextM ()
deleteLevels = createDeleteEntitiesFn collection

deleteLevelByUuid :: String -> AppContextM ()
deleteLevelByUuid = createDeleteEntityByFn collection "uuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindLevels callback = createHeeHelper findLevels callback
