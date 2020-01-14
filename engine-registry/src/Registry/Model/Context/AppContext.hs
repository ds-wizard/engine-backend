module Registry.Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)

import Registry.Model.Config.AppConfig
import Registry.Model.Config.BuildInfoConfig
import Registry.Model.Organization.Organization

data AppContext =
  AppContext
    { _appContextApplicationConfig :: AppConfig
    , _appContextLocalization :: M.Map String String
    , _appContextBuildInfoConfig :: BuildInfoConfig
    , _appContextPool :: ConnectionPool
    , _appContextTraceUuid :: U.UUID
    , _appContextCurrentOrganization :: Maybe Organization
    }

newtype AppContextM a =
  AppContextM
    { runAppContextM :: ReaderT AppContext (LoggingT IO) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadLogger)
