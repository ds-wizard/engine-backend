module Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import GHC.Records

import Shared.ActionKey.Database.Mapping.ActionKey.ActionKey ()
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger

entityName = "action_key"

findActionKeys
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     , FromField aType
     , FromField identity
     )
  => m [ActionKey identity aType]
findActionKeys = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findActionKeyByHash
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     , FromField aType
     , FromField identity
     )
  => String
  -> m (ActionKey identity aType)
findActionKeyByHash hash = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

findActionKeyByIdentityAndHash'
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     , FromField aType
     , FromField identity
     , ToField identity
     )
  => String
  -> String
  -> m (Maybe (ActionKey identity aType))
findActionKeyByIdentityAndHash' identity hash = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("identity", identity), ("hash", hash)]

insertActionKey
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     , ToField aType
     , ToField identity
     )
  => ActionKey identity aType
  -> m Int64
insertActionKey = createInsertFn entityName

deleteActionKeys
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
  => m Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash
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
  -> m Int64
deleteActionKeyByHash hash = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

deleteActionKeyByIdentity
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
  -> m Int64
deleteActionKeyByIdentity identity = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("identity", identity)]

deleteActionKeyByIdentityAndHash
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
  -> m Int64
deleteActionKeyByIdentityAndHash identity hash = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("identity", identity), ("hash", hash)]

deleteActionKeyOlderThen
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
  => UTCTime
  -> m Int64
deleteActionKeyOlderThen date = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
