module Registry.Model.Context.BaseContext where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)
import Servant (ServerError)

import Registry.Model.Config.ServerConfig
import Shared.Model.Config.BuildInfoConfig

data BaseContext =
  BaseContext
    { _baseContextServerConfig :: ServerConfig
    , _baseContextLocalization :: M.Map String String
    , _baseContextBuildInfoConfig :: BuildInfoConfig
    , _baseContextDbPool :: Pool Connection
    , _baseContextS3Client :: MinioConn
    }

newtype BaseContextM a =
  BaseContextM
    { runBaseContextM :: ReaderT BaseContext (LoggingT (ExceptT ServerError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadError ServerError, MonadLogger)
