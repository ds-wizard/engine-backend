module Shared.Prefab.Database.Migration.Development.Prefab.PrefabMigration where

import Shared.Common.Constant.Component
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import Shared.Prefab.Database.Migration.Development.Prefab.Data.Prefabs

runMigration :: AppContextC sc s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Prefab/Prefab) started"
  deletePrefabs
  insertPrefab kmIntegrationBioPortalPrefab
  insertPrefab authServicePrefab
  insertPrefab differentPrefab
  logInfo _CMP_MIGRATION "(Prefab/Prefab) ended"
