module Shared.Common.Model.Context.BaseContext where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant (ServerError)

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig

class
  ( HasField "dbPool'" context (Pool Connection)
  , HasField "s3Client'" context MinioConn
  , HasField "serverConfig'" context sc
  , HasField "cloud'" sc ServerConfigCloud
  , HasField "s3'" sc ServerConfigS3
  , HasField "sentry'" sc ServerConfigSentry
  , HasField "buildInfoConfig'" context BuildInfoConfig
  , HasField "httpClientManager'" context Manager
  , HasField "logging" sc ServerConfigLogging
  ) =>
  BaseContextType context sc

class
  ( MonadLogger m
  , MonadIO m
  , MonadError ServerError m
  , MonadReader context m
  , BaseContextType context sc
  ) =>
  BaseContextC context sc m
