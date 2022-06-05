module Shared.Model.Context.AppContext where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data AppContext =
  AppContext
    { _appContextDbConnection :: Connection
    , _appContextS3Client :: MinioConn
    , _appContextLocalization :: M.Map String String
    , _appContextIdentityUuid :: Maybe String
    , _appContextTraceUuid :: U.UUID
    , _appContextAppUuid :: U.UUID
    }
