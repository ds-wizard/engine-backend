module Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)

import Api.Resource.User.UserDTO
import Model.Config.AppConfig

data AppContext = AppContext
  { _appContextConfig :: AppConfig
  , _appContextPool :: ConnectionPool
  , _appContextMsgChannel :: Maybe Channel
  , _appContextTraceUuid :: U.UUID
  , _appContextCurrentUser :: Maybe UserDTO
  }

newtype AppContextM a = AppContextM
  { runAppContextM :: ReaderT AppContext (LoggingT IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadLogger)
