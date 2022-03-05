module Wizard.Database.DAO.Prefab.PrefabDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Prefab.Prefab ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Prefab.Prefab

entityName = "prefab"

findPrefabs :: AppContextM [Prefab]
findPrefabs = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findPrefabsFiltered :: [(String, String)] -> AppContextM [Prefab]
findPrefabsFiltered queryParams = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

insertPrefab :: Prefab -> AppContextM Int64
insertPrefab = createInsertFn entityName

deletePrefabs :: AppContextM Int64
deletePrefabs = createDeleteEntitiesFn entityName
