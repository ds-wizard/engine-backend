module Wizard.Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)
import Network.HTTP.Client (Manager)

import Shared.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Config.BuildInfoConfig
import Wizard.Model.Config.ServerConfig

data AppContext =
  AppContext
    { _appContextApplicationConfig :: ServerConfig
    , _appContextLocalization :: M.Map String String
    , _appContextBuildInfoConfig :: BuildInfoConfig
    , _appContextPool :: ConnectionPool
    , _appContextMsgChannel :: Maybe Channel
    , _appContextHttpClientManager :: Manager
    , _appContextTraceUuid :: U.UUID
    , _appContextCurrentUser :: Maybe UserDTO
    , _appContextShutdownFlag :: MVar ()
    }

newtype AppContextM a =
  AppContextM
    { runAppContextM :: ReaderT AppContext (LoggingT (ExceptT AppError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError AppError, MonadLogger)
