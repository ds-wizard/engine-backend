module Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Util.String (trim)
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.PersistentCommand.PersistentCommand ()
import Wizard.Database.Mapping.PersistentCommand.PersistentCommandSimple ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.PersistentCommand.PersistentCommandSimple
import Wizard.Util.Logger

entityName = "persistent_command"

channelName = "persistent_command_channel"

pageLabel = "persistentCommands"

findPersistentCommands :: AppContextM [PersistentCommand]
findPersistentCommands = createFindEntitiesFn entityName

findPersistentCommandsPage :: [String] -> Pageable -> [Sort] -> AppContextM (Page PersistentCommand)
findPersistentCommandsPage states pageable sort = do
  logInfoU "" (show states)
  let condition =
        case states of
          [] -> ""
          _ -> f' "WHERE state in (%s) " [generateQuestionMarks states]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition states

findPersistentCommandsByStates :: AppContextM [PersistentCommandSimple]
findPersistentCommandsByStates = do
  let sql =
        "SELECT uuid, app_uuid, created_by \
          \FROM persistent_command \
          \WHERE (state = 'NewPersistentCommandState' \
          \  OR (state = 'ErrorPersistentCommandState' AND attempts <= max_attempts AND updated_at < (now() - (2 ^ attempts - 1) * INTERVAL '1 min'))) \
          \  AND internal = true"
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findPersistentCommandByUuid :: String -> AppContextM PersistentCommand
findPersistentCommandByUuid uuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", uuid)]

findPersistentCommandSimpleByUuid :: String -> AppContextM PersistentCommandSimple
findPersistentCommandSimpleByUuid uuid =
  createFindEntityWithFieldsByFn "uuid, app_uuid, created_by" False entityName [("uuid", uuid)]

insertPersistentCommand :: PersistentCommand -> AppContextM Int64
insertPersistentCommand command = do
  createInsertFn entityName command
  if command ^. internal
    then notifyPersistentCommandQueue
    else notifySpecificPersistentCommandQueue command

updatePersistentCommandById :: PersistentCommand -> AppContextM Int64
updatePersistentCommandById command = do
  let sql =
        fromString
          "UPDATE persistent_command SET uuid = ?, state = ?, component = ?, function = ?, body = ?, last_error_message = ?, attempts = ?, max_attempts = ?, app_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, internal = ? WHERE uuid = ?"
  let params = toRow command ++ [toField . U.toText $ command ^. uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePersistentCommands :: AppContextM Int64
deletePersistentCommands = createDeleteEntitiesFn entityName

deletePersistentCommandByUuid :: String -> AppContextM Int64
deletePersistentCommandByUuid uuid = createDeleteEntityByFn entityName [("uuid", uuid)]

deletePersistentCommandByCreatedBy :: String -> AppContextM Int64
deletePersistentCommandByCreatedBy createdBy = createDeleteEntityByFn entityName [("created_by", createdBy)]

listenPersistentCommandChannel :: AppContextM ()
listenPersistentCommandChannel = createChannelListener channelName

createChannelListener :: String -> AppContextM ()
createChannelListener name = do
  let sql = f' "listen %s" [name]
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  logInfoU _CMP_DATABASE (f' "Listening for '%s' channel" [name])

getChannelNotification :: AppContextM Notification
getChannelNotification = do
  logInfoU _CMP_DATABASE "Waiting for new notification"
  notification <- runDB getNotification
  logInfoU _CMP_DATABASE (f' "Receiving notification for channel '%s'" [BS.unpack . notificationChannel $ notification])
  return notification

notifyPersistentCommandQueue :: AppContextM Int64
notifyPersistentCommandQueue = do
  let sql = f' "NOTIFY %s" [channelName]
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

notifySpecificPersistentCommandQueue :: PersistentCommand -> AppContextM Int64
notifySpecificPersistentCommandQueue command = do
  let sql = f' "NOTIFY %s__%s, '%s'" [channelName, command ^. component, U.toString $ command ^. uuid]
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
