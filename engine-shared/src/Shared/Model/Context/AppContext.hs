module Shared.Model.Context.AppContext where

import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data AppContext = AppContext
  { dbPool :: Pool Connection
  , dbConnection :: Maybe Connection
  , s3Client :: MinioConn
  , localization :: M.Map String String
  , identityUuid :: Maybe String
  , traceUuid :: U.UUID
  , appUuid :: U.UUID
  }
