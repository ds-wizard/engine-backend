module Model.Context.BaseContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)

import Model.Config.AppConfig

data BaseContext = BaseContext
  { _baseContextConfig :: AppConfig
  , _baseContextPool :: ConnectionPool
  , _baseContextMsgChannel :: Maybe Channel
  }

newtype BaseContextM a = BaseContextM
  { runBaseContextM :: ReaderT BaseContext (LoggingT IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadLogger)
