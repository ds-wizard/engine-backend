module Wizard.Service.Prefab.PrefabService where

import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Prefab.PrefabDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Prefab.Prefab

getPrefabsFiltered :: [(String, String)] -> AppContextM [Prefab]
getPrefabsFiltered queryParams = runInTransaction $ findPrefabsFiltered queryParams
