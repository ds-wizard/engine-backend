module Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data PersistentCommandState
  = NewPersistentCommandState
  | DonePersistentCommandState
  | ErrorPersistentCommandState
  | IgnorePersistentCommandState
  deriving (Show, Eq, Generic, Read)

data PersistentCommand identity = PersistentCommand
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , body :: String
  , lastTraceUuid :: Maybe U.UUID
  , lastErrorMessage :: Maybe String
  , attempts :: Int
  , maxAttempts :: Int
  , internal :: Bool
  , destination :: Maybe String
  , tenantUuid :: U.UUID
  , createdBy :: Maybe identity
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
