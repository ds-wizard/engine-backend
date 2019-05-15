module Database.DAO.Level.LevelDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, find, findOne, insert, rest, select)

import Database.BSON.Level.Level ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Level.Level

entityName = "level"

levelCollection = "levels"

findLevels :: AppContextM (Either AppError [Level])
findLevels = do
  let action = rest =<< find (select [] levelCollection)
  levelsS <- runDB action
  return . deserializeEntities $ levelsS

findLevelByShortUuid :: String -> AppContextM (Either AppError Level)
findLevelByShortUuid uuid = do
  let action = findOne $ select ["uuid" =: uuid] levelCollection
  maybeLevelS <- runDB action
  return . deserializeMaybeEntity entityName uuid $ maybeLevelS

insertLevel :: Level -> AppContextM Value
insertLevel level = do
  let action = insert levelCollection (toBSON level)
  runDB action

deleteLevels :: AppContextM ()
deleteLevels = do
  let action = delete $ select [] levelCollection
  runDB action

deleteLevelByUuid :: String -> AppContextM ()
deleteLevelByUuid uuid = do
  let action = deleteOne $ select ["uuid" =: uuid] levelCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindLevels callback = do
  eitherLevels <- findLevels
  case eitherLevels of
    Right levels -> callback levels
    Left error -> return . Left $ error
