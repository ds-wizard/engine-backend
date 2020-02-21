module Wizard.Service.Level.LevelService where

import Wizard.Api.Resource.Level.LevelDTO
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Level.LevelMapper

getLevels :: AppContextM [LevelDTO]
getLevels = do
  levels <- findLevels
  return . fmap toLevelDTO $ levels
