module Wizard.Model.Context.BaseContext where

import Control.Applicative (Applicative)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)
import Network.HTTP.Client (Manager)
import Servant (ServerError)
import Servant.Client (ClientEnv)

import Shared.Model.Config.BuildInfoConfig
import Wizard.Model.Config.ServerConfig

data BaseContext =
  BaseContext
    { _baseContextServerConfig :: ServerConfig
    , _baseContextLocalization :: M.Map String String
    , _baseContextBuildInfoConfig :: BuildInfoConfig
    , _baseContextPool :: ConnectionPool
    , _baseContextMsgChannel :: Maybe Channel
    , _baseContextHttpClientManager :: Manager
    , _baseContextRegistryClient :: ClientEnv
    , _baseContextShutdownFlag :: MVar ()
    }

newtype BaseContextM a =
  BaseContextM
    { runBaseContextM :: ReaderT BaseContext (LoggingT (ExceptT ServerError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadError ServerError, MonadLogger)
