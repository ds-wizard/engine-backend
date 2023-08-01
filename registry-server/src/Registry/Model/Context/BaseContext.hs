module Registry.Model.Context.BaseContext where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant (ServerError)

import Registry.Model.Config.ServerConfig
import Shared.Common.Model.Config.BuildInfoConfig

data BaseContext = BaseContext
  { serverConfig :: ServerConfig
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  }

newtype BaseContextM a = BaseContextM {runBaseContextM :: ReaderT BaseContext (LoggingT (ExceptT ServerError IO)) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadError ServerError, MonadLogger)
