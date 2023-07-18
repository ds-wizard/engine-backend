module Shared.Prefab.Database.DAO.Prefab.PrefabDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import Shared.Prefab.Database.Mapping.Prefab.Prefab ()
import Shared.Prefab.Model.Prefab.Prefab

entityName = "prefab"

findPrefabs
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m [Prefab]
findPrefabs = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findPrefabsFiltered
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => [(String, String)]
  -> m [Prefab]
findPrefabsFiltered queryParams = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

insertPrefab
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => Prefab
  -> m Int64
insertPrefab = createInsertFn entityName

deletePrefabs
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
deletePrefabs = createDeleteEntitiesFn entityName
