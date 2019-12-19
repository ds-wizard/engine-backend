module Wizard.Service.Level.LevelService where

import Shared.Model.Error.Error
import Wizard.Api.Resource.Level.LevelDTO
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Level.LevelMapper

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
