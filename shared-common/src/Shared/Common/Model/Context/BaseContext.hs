module Shared.Common.Model.Context.BaseContext where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data BaseContext = BaseContext
  { dbPool :: Pool Connection
  , s3Client :: MinioConn
  }
