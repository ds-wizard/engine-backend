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

data BaseContext = BaseContext
  { serverConfig :: ServerConfig
  , localization :: M.Map String String
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  , registryClient :: ClientEnv
  , shutdownFlag :: MVar ()
  , cache :: ServerCache
  }

newtype BaseContextM a = BaseContextM
  { runBaseContextM :: ReaderT BaseContext (LoggingT (ExceptT ServerError IO)) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadError ServerError, MonadLogger)
