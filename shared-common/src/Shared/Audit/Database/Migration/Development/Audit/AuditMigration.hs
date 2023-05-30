module Shared.Audit.Database.Migration.Development.Audit.AuditMigration where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Audit.Database.DAO.Audit.AuditDAO
import Shared.Common.Constant.Component
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger

runMigration
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Audit/Audit) started"
  deleteAudits
  logInfo _CMP_MIGRATION "(Audit/Audit) ended"
