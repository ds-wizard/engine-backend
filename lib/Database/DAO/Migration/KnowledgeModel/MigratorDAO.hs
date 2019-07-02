module Database.DAO.Migration.KnowledgeModel.MigratorDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, fetch, findOne, insert, merge, save, select)

import Database.BSON.Migration.KnowledgeModel.MigratorState ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Migration.KnowledgeModel.MigratorState

entityName = "kmMigration"

msCollection = "kmMigrations"

findMigratorStateByBranchUuid :: String -> AppContextM (Either AppError MigratorState)
findMigratorStateByBranchUuid uuid = do
  let action = findOne $ select ["branchUuid" =: uuid] msCollection
  maybeMigratorState <- runDB action
  return . deserializeMaybeEntity entityName uuid $ maybeMigratorState

insertMigratorState :: MigratorState -> AppContextM Value
insertMigratorState ms = do
  let action = insert msCollection (toBSON ms)
  runDB action

updateMigratorState :: MigratorState -> AppContextM ()
updateMigratorState ms = do
  let msBranchUuid = ms ^. branchUuid
  let action = fetch (select ["branchUuid" =: msBranchUuid] msCollection) >>= save msCollection . merge (toBSON ms)
  runDB action

deleteMigratorStates :: AppContextM ()
deleteMigratorStates = do
  let action = delete $ select [] msCollection
  runDB action

deleteMigratorStateByBranchUuid :: String -> AppContextM ()
deleteMigratorStateByBranchUuid branchUuid = do
  let action = delete $ select ["branchUuid" =: branchUuid] msCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindMigratorStateByBranchUuid branchUuid callback = do
  eitherMigratorState <- findMigratorStateByBranchUuid branchUuid
  case eitherMigratorState of
    Right migratorState -> callback migratorState
    Left error -> return . Left $ error

hmFindMigratorStateByBranchUuid branchUuid callback = do
  eitherMigratorState <- findMigratorStateByBranchUuid branchUuid
  case eitherMigratorState of
    Right migratorState -> callback migratorState
    Left error -> return . Just $ error
