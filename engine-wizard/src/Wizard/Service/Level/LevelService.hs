module Wizard.Service.Level.LevelService where

import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Level.Level

getLevels :: AppContextM [Level]
getLevels = findLevels
