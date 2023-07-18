module Shared.Prefab.Service.Prefab.PrefabService where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Common.Model.Error.Error
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import Shared.Prefab.Model.Prefab.Prefab

getPrefabsFiltered
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
getPrefabsFiltered = findPrefabsFiltered
