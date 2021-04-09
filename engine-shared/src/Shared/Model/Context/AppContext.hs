module Shared.Model.Context.AppContext where

import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data AppContext =
  AppContext
    { _appContextPool :: ConnectionPool
    , _appContextDbPool :: Pool Connection
    , _appContextS3Client :: MinioConn
    , _appContextLocalization :: M.Map String String
    , _appContextTraceUuid :: U.UUID
    }
