module Shared.PersistentCommand.Database.Migration.Development.PersistentCommand.PersistentCommandMigration where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Common.Constant.Component
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO

runMigration
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => m ()
runMigration = do
  logInfoI _CMP_MIGRATION "(PersistentCommand/PersistentCommand) started"
  deletePersistentCommands
  logInfoI _CMP_MIGRATION "(PersistentCommand/PersistentCommand) ended"
