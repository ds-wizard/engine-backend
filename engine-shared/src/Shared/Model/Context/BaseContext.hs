module Shared.Model.Context.BaseContext where

import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data BaseContext =
  BaseContext
    { _baseContextDbPool :: Pool Connection
    , _baseContextS3Client :: MinioConn
    , _baseContextLocalization :: M.Map String String
    }
