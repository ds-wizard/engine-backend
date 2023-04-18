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

data PersistentCommand = PersistentCommand
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , body :: String
  , lastErrorMessage :: Maybe String
  , attempts :: Int
  , maxAttempts :: Int
  , internal :: Bool
  , appUuid :: U.UUID
  , createdBy :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
