module Shared.Prefab.Database.DAO.Prefab.PrefabDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Prefab.Database.Mapping.Prefab.Prefab ()
import Shared.Prefab.Model.Prefab.Prefab

entityName = "prefab"

findPrefabs :: AppContextC s sc m => m [Prefab]
findPrefabs = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findPrefabsFiltered :: AppContextC s sc m => [(String, String)] -> m [Prefab]
findPrefabsFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

insertPrefab :: AppContextC s sc m => Prefab -> m Int64
insertPrefab = createInsertFn entityName

deletePrefabs :: AppContextC s sc m => m Int64
deletePrefabs = createDeleteEntitiesFn entityName
