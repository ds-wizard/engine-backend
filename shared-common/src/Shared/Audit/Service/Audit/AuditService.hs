module Shared.Audit.Service.Audit.AuditService where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Pool
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Audit.Database.DAO.Audit.AuditDAO
import Shared.Audit.Service.Audit.AuditMapper
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid

logAudit
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
  => String
  -> String
  -> String
  -> m ()
logAudit component action entity = logAuditWithBody component action entity M.empty

logAuditWithBody
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
  => String
  -> String
  -> String
  -> M.Map String String
  -> m ()
logAuditWithBody component action entity body = do
  aUuid <- liftIO generateUuid
  appUuid <- asks (.appUuid')
  createdBy <- asks (.identity')
  now <- liftIO getCurrentTime
  let audit = toAudit aUuid component action entity body (fmap (fromJust . U.fromString) createdBy) appUuid now
  insertAudit audit
  return ()
