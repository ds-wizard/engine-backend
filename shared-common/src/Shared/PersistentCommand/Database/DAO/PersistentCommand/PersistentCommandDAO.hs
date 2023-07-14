module Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Char8 as BS
import Data.Pool
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.Common.Util.String (trim)
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommandSimple ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

entityName = "persistent_command"

channelName = "persistent_command_channel"

pageLabel = "persistentCommands"

findPersistentCommands
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , FromField identity
     )
  => m [PersistentCommand identity]
findPersistentCommands = createFindEntitiesFn entityName

findPersistentCommandsPage
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , FromField identity
     )
  => [String]
  -> Pageable
  -> [Sort]
  -> m (Page (PersistentCommand identity))
findPersistentCommandsPage states pageable sort = do
  logInfoI "" (show states)
  let condition =
        case states of
          [] -> ""
          _ -> f' "WHERE state in (%s) " [generateQuestionMarks states]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition states

findPersistentCommandsByStates
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , FromField identity
     )
  => m [PersistentCommandSimple identity]
findPersistentCommandsByStates = do
  let sql =
        "SELECT uuid, destination, app_uuid, created_by \
        \FROM persistent_command \
        \WHERE (state = 'NewPersistentCommandState' \
        \  OR (state = 'ErrorPersistentCommandState' AND attempts < max_attempts AND updated_at < (now() - (2 ^ attempts - 1) * INTERVAL '1 min'))) \
        \  AND internal = true \
        \ORDER BY created_at \
        \LIMIT 5 \
        \FOR UPDATE"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findPersistentCommandByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , FromField identity
     )
  => U.UUID
  -> m (PersistentCommand identity)
findPersistentCommandByUuid uuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", U.toString uuid)]

findPersistentCommandByUuid'
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , FromField identity
     )
  => U.UUID
  -> m (Maybe (PersistentCommand identity))
findPersistentCommandByUuid' uuid = createFindEntityWithFieldsByFn' "*" entityName [("uuid", U.toString uuid)]

findPersistentCommandSimpleByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , FromField identity
     )
  => U.UUID
  -> m (PersistentCommandSimple identity)
findPersistentCommandSimpleByUuid uuid =
  createFindEntityWithFieldsByFn "uuid, destination, app_uuid, created_by" False entityName [("uuid", U.toString uuid)]

insertPersistentCommand
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , ToField identity
     )
  => PersistentCommand identity
  -> m Int64
insertPersistentCommand command = do
  createInsertFn entityName command
  if command.internal
    then notifyPersistentCommandQueue
    else notifySpecificPersistentCommandQueue command

updatePersistentCommandByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     , ToField identity
     )
  => PersistentCommand identity
  -> m Int64
updatePersistentCommandByUuid command = do
  let sql =
        fromString
          "UPDATE persistent_command SET uuid = ?, state = ?, component = ?, function = ?, body = ?, last_error_message = ?, attempts = ?, max_attempts = ?, app_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, internal = ?, destination = ? WHERE uuid = ?"
  let params = toRow command ++ [toField . U.toText $ command.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePersistentCommands
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
deletePersistentCommands = createDeleteEntitiesFn entityName

deletePersistentCommandsByCreatedBy
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => [U.UUID]
  -> m Int64
deletePersistentCommandsByCreatedBy createdBys = createDeleteEntityLikeFn entityName "created_by" (fmap U.toString createdBys)

deletePersistentCommandByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m Int64
deletePersistentCommandByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]

deletePersistentCommandByCreatedBy
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m Int64
deletePersistentCommandByCreatedBy createdBy = createDeleteEntityByFn entityName [("created_by", U.toString createdBy)]

listenPersistentCommandChannel
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
listenPersistentCommandChannel = createChannelListener channelName

createChannelListener
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m ()
createChannelListener name = do
  let sql = f' "listen %s" [name]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  logInfoI _CMP_DATABASE (f' "Listening for '%s' channel" [name])

getChannelNotification
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => m Notification
getChannelNotification = do
  logInfoI _CMP_DATABASE "Waiting for new notification"
  notification <- runDB getNotification
  logInfoI _CMP_DATABASE (f' "Receiving notification for channel '%s'" [BS.unpack . notificationChannel $ notification])
  return notification

notifyPersistentCommandQueue
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
notifyPersistentCommandQueue = do
  let sql = f' "NOTIFY %s" [channelName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

notifySpecificPersistentCommandQueue
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , MonadIO m
     )
  => PersistentCommand identity
  -> m Int64
notifySpecificPersistentCommandQueue command = do
  let sql = f' "NOTIFY %s__%s, '%s'" [channelName, command.component, U.toString command.uuid]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
