module Registry.Model.Context.AppContext where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

import Registry.Model.Config.ServerConfig
import Registry.Model.Organization.Organization
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Error.Error

data AppContext =
  AppContext
    { _appContextServerConfig :: ServerConfig
    , _appContextLocalization :: M.Map String String
    , _appContextBuildInfoConfig :: BuildInfoConfig
    , _appContextDbPool :: Pool Connection
    , _appContextTraceUuid :: U.UUID
    , _appContextCurrentOrganization :: Maybe Organization
    , _appContextS3Client :: MinioConn
    }

newtype AppContextM a =
  AppContextM
    { runAppContextM :: ReaderT AppContext (LoggingT (ExceptT AppError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError AppError, MonadLogger)
