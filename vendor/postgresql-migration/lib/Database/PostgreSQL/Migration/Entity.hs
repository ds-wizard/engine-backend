module Database.PostgreSQL.Migration.Entity where

import Control.Monad.Logger (LoggingT)
import Data.Pool (Pool)
import Data.Time
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics

_STARTED = "Started"

_DONE = "Finished"

type Error = String

type MigrationDefinition = (MigrationMeta, Pool Connection -> LoggingT IO (Maybe Error))

data MigrationMeta =
  MigrationMeta
    { mmNumber :: Int
    , mmName :: String
    , mmDescription :: String
    }
  deriving (Show, Eq, Generic)

type MigrationState = String

data MigrationRecord =
  MigrationRecord
    { mrNumber :: Int
    , mrName :: String
    , mrDescription :: String
    , mrState :: MigrationState
    , mrCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
