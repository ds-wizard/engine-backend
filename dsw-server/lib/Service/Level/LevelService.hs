module Service.Level.LevelService where

import Api.Resource.Level.LevelDTO
import Database.DAO.Level.LevelDAO
import Model.Context.AppContext
import Model.Error.Error
import Service.Level.LevelMapper

getLevels :: AppContextM (Either AppError [LevelDTO])
getLevels = heFindLevels $ \levels -> return . Right $ toLevelDTO <$> levels

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetLevels callback = do
  eitherLevels <- getLevels
  case eitherLevels of
    Right levels -> callback levels
    Left error -> return . Left $ error
