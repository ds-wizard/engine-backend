module Shared.Common.Model.Context.AppContext where

import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data AppContext = AppContext
  { dbPool :: Pool Connection
  , dbConnection :: Maybe Connection
  , s3Client :: MinioConn
  , identityUuid :: Maybe String
  , traceUuid :: U.UUID
  , appUuid :: U.UUID
  }
