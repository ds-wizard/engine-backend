module Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Class (MonadTrans)
import Database.Persist.MongoDB (ConnectionPool)

import Common.Context
import Model.Config.DSWConfig

data AppContext = AppContext
  { _appContextConfig :: DSWConfig
  , _appContextPool :: ConnectionPool
  , _appContextOldContext :: Context
  }

newtype AppContextM a = AppContextM
  { runAppContextM :: ReaderT AppContext (LoggingT IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadLogger)
