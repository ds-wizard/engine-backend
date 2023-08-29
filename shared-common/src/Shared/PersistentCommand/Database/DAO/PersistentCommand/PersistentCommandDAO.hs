module Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO where

import qualified Data.ByteString.Char8 as BS
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (trim)
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommandSimple ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

entityName = "persistent_command"

channelName = "persistent_command_channel"

pageLabel = "persistentCommands"

findPersistentCommands :: (AppContextC s sc m, FromField identity) => m [PersistentCommand identity]
findPersistentCommands = createFindEntitiesFn entityName

findPersistentCommandsPage :: (AppContextC s sc m, FromField identity) => [String] -> Pageable -> [Sort] -> m (Page (PersistentCommand identity))
findPersistentCommandsPage states pageable sort = do
  logInfoI "" (show states)
  let condition =
        case states of
          [] -> ""
          _ -> f' "WHERE state in (%s) " [generateQuestionMarks states]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition states

findPersistentCommandsByStates :: (AppContextC s sc m, FromField identity) => m [PersistentCommandSimple identity]
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

findPersistentCommandByUuid :: (AppContextC s sc m, FromField identity) => U.UUID -> m (PersistentCommand identity)
findPersistentCommandByUuid uuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", U.toString uuid)]

findPersistentCommandByUuid' :: (AppContextC s sc m, FromField identity) => U.UUID -> m (Maybe (PersistentCommand identity))
findPersistentCommandByUuid' uuid = createFindEntityWithFieldsByFn' "*" entityName [("uuid", U.toString uuid)]

findPersistentCommandSimpleByUuid :: (AppContextC s sc m, FromField identity) => U.UUID -> m (PersistentCommandSimple identity)
findPersistentCommandSimpleByUuid uuid =
  createFindEntityWithFieldsByFn "uuid, destination, app_uuid, created_by" False entityName [("uuid", U.toString uuid)]

insertPersistentCommand :: (AppContextC s sc m, ToField identity) => PersistentCommand identity -> m Int64
insertPersistentCommand command = do
  createInsertFn entityName command
  if command.internal
    then notifyPersistentCommandQueue
    else notifySpecificPersistentCommandQueue command

updatePersistentCommandByUuid :: (AppContextC s sc m, ToField identity) => PersistentCommand identity -> m Int64
updatePersistentCommandByUuid command = do
  let sql =
        fromString
          "UPDATE persistent_command SET uuid = ?, state = ?, component = ?, function = ?, body = ?, last_error_message = ?, attempts = ?, max_attempts = ?, app_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, internal = ?, destination = ?, last_trace_uuid = ? WHERE uuid = ?"
  let params = toRow command ++ [toField . U.toText $ command.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePersistentCommands :: AppContextC s sc m => m Int64
deletePersistentCommands = createDeleteEntitiesFn entityName

deletePersistentCommandsByCreatedBy :: AppContextC s sc m => [U.UUID] -> m Int64
deletePersistentCommandsByCreatedBy createdBys = createDeleteEntityLikeFn entityName "created_by" (fmap U.toString createdBys)

deletePersistentCommandByUuid :: AppContextC s sc m => U.UUID -> m Int64
deletePersistentCommandByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]

deletePersistentCommandByCreatedBy :: AppContextC s sc m => U.UUID -> m Int64
deletePersistentCommandByCreatedBy createdBy = createDeleteEntityByFn entityName [("created_by", U.toString createdBy)]

listenPersistentCommandChannel :: AppContextC s sc m => m ()
listenPersistentCommandChannel = createChannelListener channelName

createChannelListener :: AppContextC s sc m => String -> m ()
createChannelListener name = do
  let sql = f' "listen %s" [name]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  logInfoI _CMP_DATABASE (f' "Listening for '%s' channel" [name])

getChannelNotification :: AppContextC s sc m => m Notification
getChannelNotification = do
  logInfoI _CMP_DATABASE "Waiting for new notification"
  notification <- runDB getNotification
  logInfoI _CMP_DATABASE (f' "Receiving notification for channel '%s'" [BS.unpack . notificationChannel $ notification])
  return notification

notifyPersistentCommandQueue :: AppContextC s sc m => m Int64
notifyPersistentCommandQueue = do
  let sql = f' "NOTIFY %s" [channelName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

notifySpecificPersistentCommandQueue :: AppContextC s sc m => PersistentCommand identity -> m Int64
notifySpecificPersistentCommandQueue command = do
  let sql = f' "NOTIFY %s__%s, '%s'" [channelName, command.component, U.toString command.uuid]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
