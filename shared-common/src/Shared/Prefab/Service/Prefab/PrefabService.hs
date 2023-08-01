module Shared.Prefab.Service.Prefab.PrefabService where

import Shared.Common.Model.Context.AppContext
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import Shared.Prefab.Model.Prefab.Prefab

getPrefabsFiltered :: AppContextC s sc m => [(String, String)] -> m [Prefab]
getPrefabsFiltered = findPrefabsFiltered
