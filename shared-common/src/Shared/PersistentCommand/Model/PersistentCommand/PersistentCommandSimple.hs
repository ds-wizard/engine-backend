module Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple where

import qualified Data.UUID as U
import GHC.Generics

data PersistentCommandSimple identity = PersistentCommandSimple
  { uuid :: U.UUID
  , destination :: Maybe String
  , component :: String
  , tenantUuid :: U.UUID
  , createdBy :: Maybe identity
  }
  deriving (Show, Eq, Generic)
