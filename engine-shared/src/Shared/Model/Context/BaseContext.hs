module Shared.Model.Context.BaseContext where

import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data BaseContext = BaseContext
  { dbPool :: Pool Connection
  , s3Client :: MinioConn
  , localization :: M.Map String String
  }
