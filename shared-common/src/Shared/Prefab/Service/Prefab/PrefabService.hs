module Shared.Prefab.Service.Prefab.PrefabService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Uuid
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import Shared.Prefab.Model.PersistentCommand.Prefab.CreateOrUpdatePrefabCommand
import Shared.Prefab.Model.Prefab.Prefab
import Shared.Prefab.Service.Prefab.PrefabMapper

getPrefabsFiltered :: AppContextC s sc m => [(String, String)] -> m [Prefab]
getPrefabsFiltered = findPrefabsFiltered

createPrefab :: AppContextC s sc m => CreateOrUpdatePrefabCommand -> m Int64
createPrefab command = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let prefab = fromCommandCreate prefab command tenantUuid now
  insertPrefab prefab

modifyPrefab :: AppContextC s sc m => CreateOrUpdatePrefabCommand -> m Int64
modifyPrefab command = do
  uuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  prefab <- findPrefabByUuid command.uuid
  let updatedPrefab = fromCommandChange prefab command now
  updatePrefabByUuid updatedPrefab

deletePrefab :: AppContextC s sc m => U.UUID -> m Int64
deletePrefab = deletePrefabByUuid
