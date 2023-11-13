module Shared.Common.Model.Context.AppContext where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Service.Acl.AclService

class
  ( MonadLogger m
  , MonadIO m
  , MonadError AppError m
  , MonadReader s m
  , HasField "dbPool'" s (Pool Connection)
  , HasField "dbConnection'" s (Maybe Connection)
  , HasField "s3Client'" s MinioConn
  , HasField "identity'" s (Maybe String)
  , HasField "traceUuid'" s U.UUID
  , HasField "tenantUuid'" s U.UUID
  , HasField "serverConfig'" s sc
  , HasField "cloud'" sc ServerConfigCloud
  , HasField "s3'" sc ServerConfigS3
  , HasField "sentry'" sc ServerConfigSentry
  , HasField "buildInfoConfig'" s BuildInfoConfig
  , HasField "httpClientManager'" s Manager
  , AclContext m
  ) =>
  AppContextC s sc m
