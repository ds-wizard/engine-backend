module Shared.ActionKey.Service.ActionKey.ActionKeyService where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, liftIO)
import Data.Pool
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records

import Database.PostgreSQL.Simple.ToField
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Date
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid

createActionKey
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
  => identity
  -> aType
  -> U.UUID
  -> m (ActionKey identity aType)
createActionKey identity actionType appUuid = do
  hash <- liftIO generateUuid
  createActionKeyWithHash identity actionType appUuid (U.toString hash)

createActionKeyWithHash
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
  => identity
  -> aType
  -> U.UUID
  -> String
  -> m (ActionKey identity aType)
createActionKeyWithHash identity actionType appUuid hash =
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let actionKey =
          ActionKey
            { uuid = uuid
            , identity = identity
            , aType = actionType
            , hash = hash
            , appUuid = appUuid
            , createdAt = now
            }
    insertActionKey actionKey
    return actionKey

cleanActionKeys
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
cleanActionKeys = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1)
  let dayBefore = addUTCTime timeDelta now
  deleteActionKeyOlderThen dayBefore
  return ()
