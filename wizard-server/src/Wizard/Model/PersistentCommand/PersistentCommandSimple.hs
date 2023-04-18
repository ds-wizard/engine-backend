module Wizard.Model.PersistentCommand.PersistentCommandSimple where

import qualified Data.UUID as U
import GHC.Generics

data PersistentCommandSimple = PersistentCommandSimple
  { uuid :: U.UUID
  , appUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
