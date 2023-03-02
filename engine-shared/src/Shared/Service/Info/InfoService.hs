module Shared.Service.Info.InfoService where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Api.Resource.Info.InfoDTO
import Shared.Database.DAO.Component.ComponentDAO
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Error.Error

getInfo
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "buildInfoConfig'" s BuildInfoConfig
     , MonadIO m
     )
  => m InfoDTO
getInfo = do
  buildInfoConfig <- asks (.buildInfoConfig')
  components <- findComponents
  return
    InfoDTO
      { name = buildInfoConfig.name
      , version = buildInfoConfig.version
      , builtAt = buildInfoConfig.builtAt
      , components = components
      }
