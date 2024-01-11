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
  ( HasField "dbPool'" context (Pool Connection)
  , HasField "dbConnection'" context (Maybe Connection)
  , HasField "s3Client'" context MinioConn
  , HasField "identity'" context (Maybe String)
  , HasField "traceUuid'" context U.UUID
  , HasField "tenantUuid'" context U.UUID
  , HasField "serverConfig'" context sc
  , HasField "cloud'" sc ServerConfigCloud
  , HasField "s3'" sc ServerConfigS3
  , HasField "persistentCommand'" sc ServerConfigPersistentCommand
  , HasField "sentry'" sc ServerConfigSentry
  , HasField "buildInfoConfig'" context BuildInfoConfig
  , HasField "httpClientManager'" context Manager
  ) =>
  AppContextType context sc

class
  ( MonadLogger m
  , MonadIO m
  , MonadError AppError m
  , MonadReader context m
  , AppContextType context sc
  , AclContext m
  ) =>
  AppContextC context sc m
