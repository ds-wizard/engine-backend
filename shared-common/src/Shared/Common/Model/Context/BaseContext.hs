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
  ( MonadLogger m
  , MonadIO m
  , MonadError ServerError m
  , MonadReader s m
  , HasField "dbPool'" s (Pool Connection)
  , HasField "s3Client'" s MinioConn
  , HasField "serverConfig'" s sc
  , HasField "cloud'" sc ServerConfigCloud
  , HasField "s3'" sc ServerConfigS3
  , HasField "sentry'" sc ServerConfigSentry
  , HasField "buildInfoConfig'" s BuildInfoConfig
  , HasField "httpClientManager'" s Manager
  ) =>
  BaseContextC s sc m
