module Wizard.Database.Migration.Development.Prefab.PrefabMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Prefab.PrefabDAO
import Wizard.Database.Migration.Development.Prefab.Data.Prefabs
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Prefab/Prefab) started"
  deletePrefabs
  insertPrefab kmIntegrationBioPortalPrefab
  insertPrefab authServicePrefab
  insertPrefab differentPrefab
  logInfo _CMP_MIGRATION "(Prefab/Prefab) ended"
