module Model.Context.BaseContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)
import Network.HTTP.Client (Manager)

import Model.Config.AppConfig
import Model.Config.BuildInfoConfig

data BaseContext = BaseContext
  { _baseContextAppConfig :: AppConfig
  , _baseContextBuildInfoConfig :: BuildInfoConfig
  , _baseContextPool :: ConnectionPool
  , _baseContextMsgChannel :: Maybe Channel
  , _baseContextHttpClientManager :: Manager
  }

newtype BaseContextM a = BaseContextM
  { runBaseContextM :: ReaderT BaseContext (LoggingT IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadLogger)
