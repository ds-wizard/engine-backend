module Wizard.Model.Context.BaseContext where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant (ServerError)
import Servant.Client (ClientEnv)

import Shared.Model.Config.BuildInfoConfig
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig

data BaseContext =
  BaseContext
    { _baseContextServerConfig :: ServerConfig
    , _baseContextLocalization :: M.Map String String
    , _baseContextBuildInfoConfig :: BuildInfoConfig
    , _baseContextDbPool :: Pool Connection
    , _baseContextS3Client :: MinioConn
    , _baseContextHttpClientManager :: Manager
    , _baseContextRegistryClient :: ClientEnv
    , _baseContextShutdownFlag :: MVar ()
    , _baseContextCache :: ServerCache
    }

newtype BaseContextM a =
  BaseContextM
    { runBaseContextM :: ReaderT BaseContext (LoggingT (ExceptT ServerError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadError ServerError, MonadLogger)
