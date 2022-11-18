module Wizard.Model.Context.AppContext where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant.Client (ClientEnv)

import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig

data AppContext = AppContext
  { serverConfig :: ServerConfig
  , localization :: M.Map String String
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , dbConnection :: Maybe Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  , registryClient :: ClientEnv
  , traceUuid :: U.UUID
  , currentAppUuid :: U.UUID
  , currentUser :: Maybe UserDTO
  , shutdownFlag :: MVar ()
  , cache :: ServerCache
  }

newtype AppContextM a = AppContextM
  { runAppContextM :: ReaderT AppContext (LoggingT (ExceptT AppError IO)) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError AppError, MonadLogger)
