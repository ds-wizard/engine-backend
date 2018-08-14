module Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Database.Persist.MongoDB (ConnectionPool)

import Model.Config.AppConfig

data AppContext = AppContext
  { _appContextConfig :: AppConfig
  , _appContextPool :: ConnectionPool
  }

newtype AppContextM a = AppContextM
  { runAppContextM :: ReaderT AppContext (LoggingT IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadLogger)
