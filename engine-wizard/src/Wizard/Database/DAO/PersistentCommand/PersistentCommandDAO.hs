module Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.PersistentCommand.PersistentCommand ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Util.Logger

entityName = "persistent_command"

findPersistentCommands :: AppContextM [PersistentCommand]
findPersistentCommands = createFindEntitiesFn entityName

findPersistentCommandsByStatesAndComponents :: [String] -> AppContextM [U.UUID]
findPersistentCommandsByStatesAndComponents components = do
  appUuid <- asks _appContextAppUuid
  let sql =
        f'
          "SELECT uuid \
          \FROM persistent_command \
          \WHERE (state = 'NewPersistentCommandState' \
          \  OR (state = 'ErrorPersistentCommandState' AND attempts < max_attempts)) \
          \  AND component IN (%s) \
          \  AND app_uuid = '%s'"
          [L.intercalate "," . fmap (\c -> f' "'%s'" [c]) $ components, U.toString appUuid]
  logInfoU _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findPersistentCommandByUuid :: String -> AppContextM PersistentCommand
findPersistentCommandByUuid uuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", uuid)]

insertPersistentCommand :: PersistentCommand -> AppContextM Int64
insertPersistentCommand = createInsertFn entityName

updatePersistentCommandById :: PersistentCommand -> AppContextM Int64
updatePersistentCommandById command = do
  let sql =
        fromString
          "UPDATE persistent_command SET uuid = ?, state = ?, component = ?, function = ?, body = ?, last_error_message = ?, attempts = ?, max_attempts = ?, app_uuid = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
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
