module Registry.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Control.Lens ((^.))
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import LensesConfig
import Registry.Database.DAO.Common
import Registry.Database.Mapping.PersistentCommand.PersistentCommand ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Model.PersistentCommand.PersistentCommand
import Registry.Util.Logger
import Shared.Util.String (trim)

entityName = "persistent_command"

channelName = "persistent_command_channel"

findPersistentCommands :: AppContextM [PersistentCommand]
findPersistentCommands = createFindEntitiesFn entityName

findPersistentCommandByUuid :: String -> AppContextM PersistentCommand
findPersistentCommandByUuid uuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", uuid)]

insertPersistentCommand :: PersistentCommand -> AppContextM Int64
insertPersistentCommand command = do
  createInsertFn entityName command
  notifySpecificPersistentCommandQueue command

deletePersistentCommands :: AppContextM Int64
deletePersistentCommands = createDeleteEntitiesFn entityName

deletePersistentCommandByUuid :: String -> AppContextM Int64
deletePersistentCommandByUuid uuid = createDeleteEntityByFn entityName [("uuid", uuid)]

notifySpecificPersistentCommandQueue :: PersistentCommand -> AppContextM Int64
notifySpecificPersistentCommandQueue command = do
  let sql = f' "NOTIFY %s__%s, '%s'" [channelName, command ^. component, U.toString $ command ^. uuid]
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
