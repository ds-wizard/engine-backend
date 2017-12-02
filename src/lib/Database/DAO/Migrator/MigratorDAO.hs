module Database.DAO.Migrator.MigratorDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (findOne, select, insert, fetch, save, merge, delete, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Common.Types
import Database.BSON.Migrator.MigratorState
import Database.DAO.Common
import Model.Migrator.MigratorState

msCollection = "migrations"

findMigratorStateByBranchUuid :: Context -> String -> IO (Either AppError MigratorState)
findMigratorStateByBranchUuid context branchUuid = do
  let action = findOne $ select ["branchUuid" =: branchUuid] msCollection
  maybeMigratorState <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeMigratorState

insertMigratorState :: Context -> MigratorState -> IO Value
insertMigratorState context ms = do
  let action = insert msCollection (toBSON ms)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateMigratorState :: Context -> MigratorState -> IO ()
updateMigratorState context ms = do
  let branchUuid = ms ^. msBranchUuid
  let action = fetch (select ["branchUuid" =: branchUuid] msCollection) >>= save msCollection . merge (toBSON ms)
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteMigratorStates :: Context -> IO ()
deleteMigratorStates context = do
  let action = delete $ select [] msCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteMigratorStateByBranchUuid :: Context -> String -> IO ()
deleteMigratorStateByBranchUuid context branchUuid = do
  let action = delete $ select ["branchUuid" =: branchUuid] msCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
