module Database.PostgreSQL.Migration.Migration
  ( migrateDatabase
  ) where

import Control.Monad.Logger (LoggingT)
import Data.List
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

import Database.PostgreSQL.Migration.Database
import Database.PostgreSQL.Migration.Entity

migrateDatabase :: Pool Connection -> [MigrationDefinition] -> (String -> LoggingT IO ()) -> LoggingT IO (Maybe Error)
migrateDatabase dbPool migrationDefinitions logInfo = do
  logInfo "started"
  logInfo "ensure migration table in DB: started"
  startTransaction dbPool
  ensureMigrationTable dbPool
  logInfo "ensure migration table in DB: loaded"
  logInfo "loading migrations from DB: started"
  appliedMigrations <- getMigrationsFromDb dbPool
  logInfo "loading migrations from DB: loaded"
  result <-
    case length appliedMigrations of
      0 -> do
        logInfo $ "list of migrations to apply: " ++ numbersOfMigrations migrationDefinitions
        applyMigrations migrationDefinitions
      _ -> do
        let lastAppliedMigration = last appliedMigrations
        let newMigrationDefinitions =
              filter (\(mm, _) -> mmNumber mm > mrNumber lastAppliedMigration) migrationDefinitions
        if not (null newMigrationDefinitions)
          then do
            logInfo $ "list of migrations to apply: " ++ numbersOfMigrations newMigrationDefinitions
            applyMigrations newMigrationDefinitions
          else do
            logInfo "no new migration to apply"
            return Nothing
  case result of
    Just error -> do
      rollbackTransaction dbPool
      logInfo "rollback migrations"
    Nothing -> do
      commitTransaction dbPool
      logInfo "commit migrations"
  logInfo "ended"
  return result
  where
    applyMigrations = foldl' applyMigration (return Nothing)
    applyMigration :: LoggingT IO (Maybe Error) -> MigrationDefinition -> LoggingT IO (Maybe Error)
    applyMigration ioMaybeError (meta, migrate) = do
      maybeError <- ioMaybeError
      logInfo $ "Migration (" ++ (show . mmNumber $ meta) ++ " - " ++ mmName meta ++ "): started"
      case maybeError of
        Nothing -> do
          startMigration dbPool meta
          let result = migrate dbPool
          endMigration dbPool meta
          logInfo $ "Migration (" ++ (show . mmNumber $ meta) ++ " - " ++ mmName meta ++ "): finished"
          result
        Just error -> return . Just $ error

numbersOfMigrations :: [MigrationDefinition] -> String
numbersOfMigrations migrationDefinitions = intercalate ", " (numberOfMigration <$> migrationDefinitions)
  where
    numberOfMigration :: MigrationDefinition -> String
    numberOfMigration (mm, _) = show . mmNumber $ mm
