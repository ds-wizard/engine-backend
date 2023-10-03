module Registry.Model.Context.AppContext where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Registry.Model.Config.ServerConfig
import RegistryLib.Model.Organization.Organization
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Error.Error

data AppContext = AppContext
  { serverConfig :: ServerConfig
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , dbConnection :: Maybe Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  , traceUuid :: U.UUID
  , currentOrganization :: Maybe Organization
  }

newtype AppContextM a = AppContextM {runAppContextM :: ReaderT AppContext (LoggingT (ExceptT AppError IO)) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError AppError, MonadLogger)
