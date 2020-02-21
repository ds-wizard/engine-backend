module Wizard.Model.Context.BaseContext where

import Control.Applicative (Applicative)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)
import Network.HTTP.Client (Manager)
import Servant (ServantErr)

import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.BuildInfoConfig

data BaseContext =
  BaseContext
    { _baseContextAppConfig :: AppConfig
    , _baseContextLocalization :: M.Map String String
    , _baseContextBuildInfoConfig :: BuildInfoConfig
    , _baseContextPool :: ConnectionPool
    , _baseContextMsgChannel :: Maybe Channel
    , _baseContextHttpClientManager :: Manager
    }

newtype BaseContextM a =
  BaseContextM
    { runBaseContextM :: ReaderT BaseContext (LoggingT (ExceptT ServantErr IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader BaseContext, MonadError ServantErr, MonadLogger)
