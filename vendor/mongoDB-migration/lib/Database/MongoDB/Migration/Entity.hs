module Database.MongoDB.Migration.Entity where

import Control.Monad.Logger (LoggingT)
import Data.Time
import Database.Persist.MongoDB (ConnectionPool)

_STARTED = "Started"

_DONE = "Finished"

type Error = String

type MigrationDefinition = (MigrationMeta, ConnectionPool -> LoggingT IO (Maybe Error))

data MigrationMeta = MigrationMeta
  { mmNumber :: Int
  , mmName :: String
  , mmDescription :: String
  } deriving (Show, Eq)

type MigrationState = String

data MigrationRecord = MigrationRecord
  { mrNumber :: Int
  , mrName :: String
  , mrDescription :: String
  , mrState :: MigrationState
  , mrCreatedAt :: UTCTime
  } deriving (Show, Eq)
