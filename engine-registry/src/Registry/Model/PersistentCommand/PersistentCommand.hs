module Registry.Model.PersistentCommand.PersistentCommand where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data PersistentCommandState
  = NewPersistentCommandState
  | DonePersistentCommandState
  | ErrorPersistentCommandState
  | IgnorePersistentCommandState
  deriving (Show, Eq, Generic, Read)

data PersistentCommand =
  PersistentCommand
    { _persistentCommandUuid :: U.UUID
    , _persistentCommandState :: PersistentCommandState
    , _persistentCommandComponent :: String
    , _persistentCommandFunction :: String
    , _persistentCommandBody :: String
    , _persistentCommandLastErrorMessage :: Maybe String
    , _persistentCommandAttempts :: Int
    , _persistentCommandMaxAttempts :: Int
    , _persistentCommandInternal :: Bool
    , _persistentCommandAppUuid :: U.UUID
    , _persistentCommandCreatedBy :: String
    , _persistentCommandCreatedAt :: UTCTime
    , _persistentCommandUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
