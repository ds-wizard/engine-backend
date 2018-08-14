module Database.MongoDB.Migration.Database where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import qualified Data.Bson as BSON
import Data.Time
import Database.MongoDB
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Database.MongoDB.Migration.Entity

_MIGRATION_COLLECTION = "migrations"

toMigrationMetaBSON :: MigrationMeta -> MigrationState -> UTCTime -> Document
toMigrationMetaBSON migrationMeta state createdAt =
  [ "number" BSON.=: (mmNumber migrationMeta)
  , "name" BSON.=: (mmName migrationMeta)
  , "description" BSON.=: (mmDescription migrationMeta)
  , "state" BSON.=: state
  , "createdAt" BSON.=: (show createdAt)
  ]

fromMigrationMetaBSON :: Document -> IO MigrationRecord
fromMigrationMetaBSON doc = do
  number <- BSON.lookup "number" doc
  name <- BSON.lookup "name" doc
  description <- BSON.lookup "description" doc
  state <- BSON.lookup "state" doc
  createdAt <- BSON.lookup "createdAt" doc
  return
    MigrationRecord
    {mrNumber = number, mrName = name, mrDescription = description, mrState = state, mrCreatedAt = read createdAt}

getMigrationsFromDb :: ConnectionPool -> LoggingT IO [MigrationRecord]
getMigrationsFromDb dbPool = do
  let action = rest =<< find (select [] _MIGRATION_COLLECTION)
  docs <- runMongoDBPoolDef action dbPool
  result <- liftIO $ foldl fromBSON (return []) docs
  return result
  where
    fromBSON :: IO [MigrationRecord] -> Document -> IO [MigrationRecord]
    fromBSON ioAcc doc = do
      acc <- ioAcc
      mr <- fromMigrationMetaBSON doc
      return $ acc ++ [mr]

startMigration :: ConnectionPool -> MigrationMeta -> LoggingT IO ()
startMigration dbPool meta = do
  now <- liftIO getCurrentTime
  let action = insert _MIGRATION_COLLECTION (toMigrationMetaBSON meta _STARTED now)
  result <- runMongoDBPoolDef action dbPool
  return ()

endMigration :: ConnectionPool -> MigrationMeta -> LoggingT IO ()
endMigration dbPool meta = do
  let action = modify (select ["number" =: (mmNumber meta)] _MIGRATION_COLLECTION) ["$set" =: ["state" =: _DONE]]
  runMongoDBPoolDef action dbPool
  return ()
